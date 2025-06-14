# === Libraries ===
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(readr)


# === Load and clean data ===
df <- read_excel("volume_5.xlsx", skip = 1) %>%
  rename_with(str_trim) %>%  # Trim whitespace in column names
  mutate(
    Animal = str_extract(`Original Image Name`, "SL_16_(\\d+)") %>% str_remove("SL_16_"),
    ImageTime = str_extract(`Original Image Name`, "\\d{2}\\.\\d{2}\\.\\d{2}") %>% str_remove_all("_")
  )# === Filter and label ===
df <- df %>%
  filter(`Surpass Object` %in% c("PV DP Soma", "PV DP Process", "PV ML Soma", "PV ML Process")) %>%
  mutate(
    Type = if_else(str_detect(`Surpass Object`, "DP"), "DP", "ML"),
    Compartment = if_else(str_detect(`Surpass Object`, "Soma"), "Soma", "Process")
  )

# === Count per image and mean per animal ===
df_summary <- df %>%
  count(Animal, ImageTime, Type, Compartment, name = "Count") %>%
  group_by(Animal, Type, Compartment) %>%
  summarise(Mean_Count = mean(Count), .groups = "drop")

# === Filter animals with all 4 conditions and create 'Group' ===
df_filtered_animals <- df_summary %>%
  group_by(Animal) %>%
  filter(n() == 4) %>%
  ungroup() %>%
  mutate(
    Group = paste(Type, Compartment),
    Group = factor(Group, levels = c("DP Process", "DP Soma", "ML Process", "ML Soma"))
  )

# === Run ANOVA ===
anova_result <- anova_test(
  data = df_filtered_animals,
  dv = Mean_Count,
  wid = Animal,
  within = c(Type, Compartment)
)
print("=== ANOVA Results ===")
print(anova_result)

# === Pairwise comparisons ===
pairwise_res <- df_filtered_animals %>%
  pairwise_t_test(
    Mean_Count ~ Group,
    paired = TRUE,
    p.adjust.method = "bonferroni",
    detailed = TRUE  # ✅ this adds conf.low and conf.high
  ) %>%
  add_xy_position(x = "Group", fun = "mean")


colnames(pairwise_res)


# === Plot Colors ===
group_colors <- c(
  "DP Soma" = "#215476",
  "ML Soma" = "#43909d",
  "DP Process" = "#b3bbd2",
  "ML Process" = "#e2cde8"
)

# === Create plot ===
p <- ggplot(df_filtered_animals, aes(x = Group, y = Mean_Count)) +
  geom_violin(trim = FALSE, scale = "width", color = "black", aes(fill = Group)) +
  geom_boxplot(width = 0.15, color = "black", fill = "white", outlier.shape = NA, aes(fill = Group)) +
  geom_jitter(size = 3, shape = 21, stroke = 1, color = "black", fill = "white", width = 0.15) +
  stat_pvalue_manual(pairwise_res, label = "p.adj.signif", tip.length = 0.01) +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(
    name = "Number of Punctas per Compartment per Animal",
    breaks = pretty(df_filtered_animals$Mean_Count, n = 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  xlab("") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),  # ✅ REMOVE GREY GRID
    axis.line = element_line(size = 1.2, color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )



# === Show and save plot ===
print(p)
ggsave("DP_ML_count_distribution_comparison_ANOVA.svg", plot = p, width = 8, height = 6, dpi = 300)

# === Debug check ===
str(pairwise_res)
str(df_filtered_animals)
colnames(df_filtered_animals)

# === Format pairwise results for CSV ===
# Compute group means
group_means <- df_filtered_animals %>%
  group_by(Group) %>%
  summarise(Mean = mean(Mean_Count), .groups = "drop")

# Merge group means into pairwise results
formatted_pairwise <- pairwise_res %>%
  left_join(group_means, by = c("group1" = "Group")) %>%
  rename(Mean_1 = Mean) %>%
  left_join(group_means, by = c("group2" = "Group")) %>%
  rename(Mean_2 = Mean) %>%
  mutate(
    Comparison = paste(group1, "vs", group2),
    `Difference (±SEM)` = sprintf("%.2f ± %.2f", 
                                  Mean_1 - Mean_2, 
                                  sd(df_filtered_animals$Mean_Count, na.rm = TRUE) / sqrt(length(unique(df_filtered_animals$Animal)))
    ),
    `95% CI` = paste0(round(conf.low, 2), " to ", round(conf.high, 2)),
    `t, df` = sprintf("t=%.3f, df=%.0f", statistic, df),
    `R² (eta squared)` = round(statistic^2 / (statistic^2 + df), 4),
    `Significant (P < 0.05)` = ifelse(p.adj < 0.05, "Yes", "No"),
    `p-value (adj)` = signif(p.adj, 4)
  ) %>%
  select(
    ANOVA = p,  # From overall ANOVA
    Comparison,
    Mean_1,
    Mean_2,
    `Difference (±SEM)`,
    `95% CI`,
    `t, df`,
    `R² (eta squared)`,
    `Significant (P < 0.05)`,
    `p-value (adj)`
  ) %>%
  mutate(ANOVA = signif(anova_result$p, 4)[1])  # Add ANOVA p-value to every row

# === Save CSV ===
write_csv(formatted_pairwise, "ANOVA_DP_ML_compartment_count_results.csv")


