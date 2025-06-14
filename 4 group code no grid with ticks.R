# Libraries
library(readxl)
library(tidyverse)
library(ggpubr)

# === Load and clean data ===
file_path <- "PV V3 SOM V3 Master sheet 4 group.xlsx"
df <- read_excel(file_path)

# Remove completely empty columns and clean column names
df <- df %>% select(where(~ !all(is.na(.))))
names(df) <- trimws(names(df))

# Convert units only if not already using µ symbol
names(df) <- gsub("(?<!µ)\\(um\\)", "(µm)", names(df), ignore.case = TRUE, perl = TRUE)
names(df) <- gsub("(?<!µ)\\(um2\\)", "(µm²)", names(df), ignore.case = TRUE, perl = TRUE)

# === Identify 4 groups and features ===
group_names <- c("PV DG", "PV CA1", "PV ERHC", "SOM")

column_info <- tibble(
  original = names(df),
  group = str_extract(names(df), paste(group_names, collapse = "|")),
  feature = str_remove(names(df), paste0("(", paste(group_names, collapse = "|"), ")\\s*"))
) %>%
  filter(!is.na(group))

# Only include features that have all 4 groups
feature_groups <- column_info %>%
  group_by(feature) %>%
  filter(all(group_names %in% group)) %>%
  group_split()

cat("🦰 Found", length(feature_groups), "valid 4-group column sets.\n")

# === Plotting Loop ===
all_t_tests <- list()
all_descriptive <- list()

for (group_set in feature_groups) {
  feature_name <- unique(group_set$feature)
  
  # Combine into long format
  temp_df <- map2_dfr(group_set$original, group_set$group, ~{
    tibble(Value = df[[.x]], Group = .y)
  })
  
  temp_df$Group <- factor(temp_df$Group, levels = group_names)
  
  # Define pairwise t-test comparisons
  comparisons <- combn(group_names, 2, simplify = FALSE)
  
  # Y-axis dynamic spacing
  y_min <- min(temp_df$Value, na.rm = TRUE)
  y_max <- max(temp_df$Value, na.rm = TRUE)
  y_range <- y_max - y_min
  step <- y_range * 0.15
  label_y_positions <- y_max + (1:length(comparisons)) * step + step * 1.5
  y_limit_upper <- y_max + (length(comparisons) + 1.5) * step
  
  # Colors
  group_colors <- c(
    "PV DG" = "#215476",
    "PV CA1" = "#43909d",
    "PV ERHC" = "#b3bbd2",
    "SOM" = "#e2cde8"
  )
  
  # === Plot ===
  p <- ggplot(temp_df, aes(x = Group, y = Value, fill = Group)) +
    geom_violin(trim = FALSE, scale = "width", color = "black") +
    geom_boxplot(width = 0.15, color = "black", fill = "white", outlier.shape = NA) +
    geom_jitter(width = 0.1, size = 2.2, shape = 21, stroke = 1, fill = "white", color = "black") +
    stat_compare_means(
      comparisons = comparisons,
      method = "t.test",
      label = "p.signif",          # This gives stars or "ns"
      label.y = label_y_positions,
      tip.length = 0.01
    ) +
    expand_limits(y = y_limit_upper) +
    scale_fill_manual(values = group_colors) +
    ylab(feature_name) +
    xlab("") +
    theme_classic(base_size = 14) +
    theme(
      # Show y-axis line and ticks
      axis.line.y = element_line(size = 0.8, color = "black"),
      axis.ticks.y = element_line(color = "black"),
      
      # Show x-axis line but remove ticks
      axis.line.x = element_line(size = 0.8, color = "black"),
      axis.ticks.x = element_blank(),             # remove only x-axis ticks
      
      # Other styling
      axis.ticks.length = unit(0.3, "cm"),
      axis.text = element_text(size = 12),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),               # remove all gridlines
      plot.title = element_blank(),
      legend.position = "none"
    )
  
  print(p)
  
  # Save plot
  safe_filename <- gsub("[^a-zA-Z0-9]", "_", feature_name)
  ggsave(filename = paste0("violin_plot_", safe_filename, ".png"),
         plot = p, width = 6, height = 5, dpi = 300)
  
  # === T-test summary ===
  # === Pairwise t-tests + ANOVA summary ===
  comparisons <- combn(group_names, 2, simplify = FALSE)
  
  # One-way ANOVA for the feature
  anova_res <- aov(Value ~ Group, data = temp_df)
  anova_summary <- summary(anova_res)[[1]]
  anova_pval <- signif(anova_summary["Pr(>F)"][1, 1], 4)
  
  # Pairwise t-tests
  t_test_results <- map_dfr(comparisons, function(comp) {
    vals1 <- temp_df %>% filter(Group == comp[1]) %>% pull(Value)
    vals2 <- temp_df %>% filter(Group == comp[2]) %>% pull(Value)
    n1 <- sum(!is.na(vals1))
    n2 <- sum(!is.na(vals2))
    
    t_res <- t.test(vals1, vals2, var.equal = FALSE)
    
    tibble(
      Feature = feature_name,
      ANOVA_p = anova_pval,
      Comparison = paste(comp[1], "vs", comp[2]),
      Mean_1 = mean(vals1, na.rm = TRUE),
      Mean_2 = mean(vals2, na.rm = TRUE),
      `Difference (±SEM)` = paste0(
        round(mean(vals1, na.rm = TRUE) - mean(vals2, na.rm = TRUE), 2),
        " ± ", round(sd(c(vals1, vals2), na.rm = TRUE) / sqrt(n1 + n2), 2)
      ),
      `95% CI` = paste0(
        round(t_res$conf.int[1], 2), " to ", round(t_res$conf.int[2], 2)
      ),
      `t, df` = paste0("t=", round(t_res$statistic, 3), ", df=", round(t_res$parameter)),
      `R² (eta²)` = round(t_res$statistic^2 / (t_res$statistic^2 + t_res$parameter), 4),
      `Significant (P < 0.05)` = ifelse(t_res$p.value < 0.05, "Yes", "No"),
      `p-value` = signif(t_res$p.value, 4)
    )
  })
  
  all_t_tests[[feature_name]] <- t_test_results
  
  
  # === Descriptive stats ===
  desc_stats <- function(x) {
    x <- x[!is.na(x)]
    tibble(
      Metric = c("N", "Min", "25%", "Median", "75%", "Max", 
                 "Mean", "SD", "SEM", "Lower 95% CI", "Upper 95% CI"),
      Value = c(
        length(x),
        min(x), quantile(x, 0.25), median(x), quantile(x, 0.75), max(x),
        mean(x), sd(x), sd(x)/sqrt(length(x)),
        t.test(x)$conf.int[1], t.test(x)$conf.int[2]
      )
    )
  }
  
  group_stats <- map_dfc(group_names, function(g) {
    vals <- temp_df %>% filter(Group == g) %>% pull(Value)
    setNames(desc_stats(vals)$Value, rep(g, 11))
  })
  
  desc_combined <- bind_cols(desc_stats(temp_df$Value)["Metric"], group_stats)
  desc_combined <- bind_rows(
    tibble(Metric = paste0("Group: ", feature_name), !!!set_names(rep(NA, length(group_names)), group_names)),
    desc_combined,
    tibble(Metric = NA, !!!set_names(rep(NA, length(group_names)), group_names))
  )
  
  all_descriptive[[feature_name]] <- desc_combined
}

# === Export summaries ===
write_csv(bind_rows(all_t_tests), "t_test_summary_4groups.csv")
write_csv(bind_rows(all_descriptive), "descriptive_stats_4groups.csv")

