library(readxl)
library(tidyverse)
library(ggpubr)
install.packages("svglite")


# Load Excel file
file_path <- "PV V3 SOM V3 Master sheet 2 group.xlsx"
df <- read_excel(file_path)

# Clean column names
names(df) <- trimws(names(df))
names(df)[is.na(names(df))] <- ""

# Function to get common title
get_common_title <- function(pv_col, som_col) {
  pv_clean <- gsub("^PV\\s*", "", pv_col)
  som_clean <- gsub("^SOM\\s*", "", som_col)
  min_len <- min(nchar(pv_clean), nchar(som_clean))
  i <- 1
  while (i <= min_len && substr(pv_clean, i, i) == substr(som_clean, i, i)) {
    i <- i + 1
  }
  common <- substr(pv_clean, 1, i - 1)
  common <- gsub("\\(um2\\)", "(µm²)", common, ignore.case = TRUE)
  return(trimws(common))
}

# Identify PV/SOM pairs
col_pairs <- list()
i <- 1
while (i <= (ncol(df) - 1)) {
  col1 <- names(df)[i]
  col2 <- names(df)[i + 1]
  if (grepl("^PV", col1, ignore.case = TRUE) && grepl("^SOM", col2, ignore.case = TRUE)) {
    col_pairs[[length(col_pairs) + 1]] <- list(pv = col1, som = col2)
    i <- i + 3
  } else {
    i <- i + 1
  }
}

cat(" Found", length(col_pairs), "PV/SOM pairs.\n")
if (length(col_pairs) == 0) stop("No valid PV/SOM column pairs found.")

# Custom color palette
group_colors <- c(
  "PV" = "#b3bbd2",
  "SOM" = "#e2cde8")

# Plot loop
for (idx in seq_along(col_pairs)) {
  pair <- col_pairs[[idx]]
  pv_name <- pair$pv
  som_name <- pair$som
  if (!(pv_name %in% names(df)) || !(som_name %in% names(df))) {
    warning(paste("Skipping pair:", pv_name, "+", som_name))
    next
  }
  
  pv_vals <- df[[pv_name]]
  som_vals <- df[[som_name]]
  common_title <- get_common_title(pv_name, som_name)
  
  temp_df <- tibble(
    Value = c(pv_vals, som_vals),
    Group = factor(rep(c("PV", "SOM"), each = length(pv_vals)), levels = c("PV", "SOM"))
  )
  
  y_min <- min(temp_df$Value, na.rm = TRUE)
  y_max <- max(temp_df$Value, na.rm = TRUE)
  y_range <- y_max - y_min
  step <- y_range * 0.15
  label_y <- y_max + step
  y_limit_upper <- y_max + 2 * step
  
  # Determine y-axis breaks (tick marks) manually
  y_breaks <- pretty(temp_df$Value, n = 5)  # adjust n = number of lines
  
  p <- ggplot(temp_df, aes(x = Group, y = Value, fill = Group)) +
    geom_violin(trim = FALSE, scale = "width", color = "black") +
    geom_boxplot(width = 0.15, color = "black", fill = "white", outlier.shape = NA) +
    geom_jitter(width = 0.1, size = 2.2, shape = 21, stroke = 1, fill = "white", color = "black") +
    stat_compare_means(method = "t.test", label = "p.signif",
                       comparisons = list(c("PV", "SOM")),
                       label.y = label_y,
                       tip.length = 0.015) +
    scale_fill_manual(values = group_colors) +
    ylab(common_title) +
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
  safe_filename <- gsub("[^a-zA-Z0-9]", "_", common_title)
  ggsave(filename = paste0("violin_plot_", safe_filename, ".svg"),
         plot = p, width = 6, height = 5, dpi = 300)
}
# T-test and summary export
t_test_results <- list()

for (pair in col_pairs) {
  pv_col <- pair$pv
  som_col <- pair$som
  pv_vals <- df[[pv_col]]
  som_vals <- df[[som_col]]
  common_title <- get_common_title(pv_col, som_col)
  
  t_res <- t.test(pv_vals, som_vals, var.equal = FALSE)
  n_pv <- sum(!is.na(pv_vals))
  n_som <- sum(!is.na(som_vals))
  
  t_test_results[[common_title]] <- tibble(
    Comparison = common_title,
    Mean_PV = mean(pv_vals, na.rm = TRUE),
    Mean_SOM = mean(som_vals, na.rm = TRUE),
    `Difference between means (±SEM)` = paste0(
      round(mean(pv_vals, na.rm = TRUE) - mean(som_vals, na.rm = TRUE), 2),
      " ± ", round(sd(c(pv_vals, som_vals), na.rm = TRUE) / sqrt(n_pv + n_som), 2)
    ),
    `95% confidence interval` = paste0(
      round(t_res$conf.int[1], 2), " to ", round(t_res$conf.int[2], 2)
    ),
    `t, df` = paste0("t=", round(t_res$statistic, 3), ", df=", round(t_res$parameter)),
    `R squared (eta squared)` = round(t_res$statistic^2 / (t_res$statistic^2 + t_res$parameter), 4),
    `Significance (P < 0.05)` = ifelse(t_res$p.value < 0.05, "Yes", "No"),
    `p-value` = signif(t_res$p.value, 4)
  )
}

t_test_summary <- bind_rows(t_test_results)
write_csv(t_test_summary, "t_test_summary_2goups.csv")
#Descriptive stats
# Descriptive stats function
desc_stats <- function(x) {
  x <- x[!is.na(x)]
  tibble(
    Metric = c("Number of values", "Minimum", "25% Percentile", "Median", "75% Percentile", "Maximum", 
               "Mean", "Std. Deviation", "Std. Error of Mean", "Lower 95% CI", "Upper 95% CI"),
    Value = c(
      length(x),
      min(x),
      quantile(x, 0.25),
      median(x),
      quantile(x, 0.75),
      max(x),
      mean(x),
      sd(x),
      sd(x)/sqrt(length(x)),
      t.test(x)$conf.int[1],
      t.test(x)$conf.int[2]
    )
  )
}

# Identify PV/SOM pairs
col_pairs <- list()
i <- 1
while (i <= (ncol(df) - 1)) {
  col1 <- names(df)[i]
  col2 <- names(df)[i + 1]
  if (grepl("^PV", col1, ignore.case = TRUE) && grepl("^SOM", col2, ignore.case = TRUE)) {
    col_pairs[[length(col_pairs) + 1]] <- list(pv = col1, som = col2)
    i <- i + 3
  } else {
    i <- i + 1
  }
}

# Get common title helper
get_common_title <- function(pv_col, som_col) {
  pv_clean <- gsub("^PV\\s*", "", pv_col)
  som_clean <- gsub("^SOM\\s*", "", som_col)
  min_len <- min(nchar(pv_clean), nchar(som_clean))
  i <- 1
  while (i <= min_len && substr(pv_clean, i, i) == substr(som_clean, i, i)) {
    i <- i + 1
  }
  common <- substr(pv_clean, 1, i - 1)
  common <- gsub("\\(um2\\)", "(µm²)", common, ignore.case = TRUE)
  return(trimws(common))
}

# Generate stats for each PV/SOM pair
descriptive_all <- bind_rows(
  lapply(col_pairs, function(pair) {
    pv_vals <- df[[pair$pv]]
    som_vals <- df[[pair$som]]
    common_title <- get_common_title(pair$pv, pair$som)
    
    # Descriptive stats tables
    pv_stats <- desc_stats(pv_vals) %>% rename(!!pair$pv := Value)
    som_stats <- desc_stats(som_vals) %>% rename(!!pair$som := Value)
    
    # Combine PV and SOM
    combined <- full_join(pv_stats, som_stats, by = "Metric")
    
    # Title row + spacer after each block
    bind_rows(
      tibble(Metric = paste0("Group: ", common_title), 
             !!pair$pv := NA, !!pair$som := NA),
      combined,
      tibble(Metric = NA, !!pair$pv := NA, !!pair$som := NA)
    )
  })
)

# Write to CSV
write_csv(descriptive_all, "descriptive_stats_2group.csv")


