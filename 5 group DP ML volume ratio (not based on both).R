# === Load Libraries ===
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(ggpubr)

# === Load Data ===
df <- read_excel("Volume_ratio.xlsx", col_names = TRUE, skip = 1)

# === Reusable Function: Paired or Unpaired T-Test Export ===
save_t_test_results <- function(group1, group2, comparison_label = "Comparison", paired = TRUE, output_file = "t_test_results.csv") {
  test_res <- t.test(group1, group2, paired = paired)
  mean1 <- mean(group1, na.rm = TRUE)
  mean2 <- mean(group2, na.rm = TRUE)
  diff_mean <- mean(group1 - group2, na.rm = TRUE)
  sem_diff <- sd(group1 - group2, na.rm = TRUE) / sqrt(length(group1))
  t_val <- as.numeric(test_res$statistic)
  df_val <- as.numeric(test_res$parameter)
  eta_sq <- t_val^2 / (t_val^2 + df_val)
  
  result <- tibble::tibble(
    Comparison = comparison_label,
    Mean_Group1 = mean1,
    Mean_Group2 = mean2,
    `Difference between means (±SEM)` = sprintf("%.2f ± %.2f", diff_mean, sem_diff),
    `95% confidence interval` = sprintf("%.2f to %.2f", test_res$conf.int[1], test_res$conf.int[2]),
    `t, df` = sprintf("t=%.3f, df=%.0f", t_val, df_val),
    `R squared (eta squared)` = round(eta_sq, 4),
    `Significance (P < 0.05)` = ifelse(test_res$p.value < 0.05, "Yes", "No"),
    `p-value` = signif(test_res$p.value, 4)
  )
  
  readr::write_csv(result, output_file)
  return(result)
}

# === Extract Animal and Image Time ===
df <- df %>%
  mutate(
    Animal = str_extract(`Original Image Name`, "SL_16_(\\d+)") %>% str_remove("SL_16_"),
    ImageTime = str_extract(`Original Image Name`, "_\\d{2}\\.\\d{2}\\.\\d{2}_") %>% str_remove_all("_")
  )

# === Filter to Process/Soma Double Positive and ML Compartments ===
df_filtered <- df %>%
  filter(`Surpass Object` %in% c("PV DP Soma", "PV DP Process", "PV ML Soma", "PV ML Process")) %>%
  mutate(
    Region = case_when(
      `Surpass Object` %in% c("PV DP Soma", "PV DP Process") ~ "DP",
      `Surpass Object` %in% c("PV ML Soma", "PV ML Process") ~ "ML"
    )
  )

# === Compute mean Volume ratio per image per region ===
volume_means <- df_filtered %>%
  group_by(Animal, ImageTime, Region) %>%
  summarise(Volume_Mean = mean(`Volume ratio`, na.rm = TRUE), .groups = "drop")

# === Compute mean Volume ratio per animal per region ===
volume_per_animal <- volume_means %>%
  group_by(Animal, Region) %>%
  summarise(Mean_Volume = mean(Volume_Mean, na.rm = TRUE), .groups = "drop")

# === Convert to wide format to filter paired animals ===
df_wide <- volume_per_animal %>%
  pivot_wider(names_from = Region, values_from = Mean_Volume) %>%
  filter(!is.na(DP) & !is.na(ML))  # Keep only animals with both DP and ML

# === Convert back to long format for plotting ===
df_long <- df_wide %>%
  pivot_longer(cols = c(DP, ML), names_to = "Group", values_to = "Value")

# === Plot Colors ===
group_colors <- c("DP" = "#215476", "ML" = "#e2cde8")

# Set label position
label_y <- max(df_long$Value, na.rm = TRUE) + 0.05

# === Final Plot ===
p <- ggplot(df_long, aes(x = Group, y = Value, fill = Group)) +
  geom_violin(trim = FALSE, scale = "width", color = "black") +
  geom_boxplot(width = 0.15, color = "black", fill = "white", outlier.shape = NA) +
  geom_point(shape = 21, color = "black", fill = "white", size = 3, stroke = 1) +
  stat_compare_means(
    method = "t.test",
    label = "p.signif",
    comparisons = list(c("ML", "DP")),
    label.y = label_y,
    tip.length = 0.01
  ) +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(
    name = "Overlapped Volume Ratio DP and ML total per Animal",
    breaks = pretty(df_long$Value, n = 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  xlab("") +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(size = 1.2, colour = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.length = unit(0.3, "cm"),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

# === Run and save paired t-test ===
save_t_test_results(
  group1 = df_wide$DP,
  group2 = df_wide$ML,
  comparison_label = "DP vs ML Volume Ratio",
  paired = TRUE,
  output_file = "Volume_Ratio_DP_vs_ML_results_updated.csv"
)

# === Save Plot ===
ggsave("Volume_Ratio_DP_vs_ML_updated.svg", plot = p, width = 6, height = 6, dpi = 300)
print(p)
