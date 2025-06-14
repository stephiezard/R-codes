# === Load Libraries ===
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(readr)

# === Load Data ===
df <- read_excel("volume_5.xlsx", col_names = TRUE, skip = 1)

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
  write_csv(result, output_file)
  return(result)
}

# === Extract Animal and Image Time ===
df <- df %>%
  mutate(
    Animal = str_extract(`Original Image Name`, "SL_16_(\\d+)") %>% str_remove("SL_16_"),
    ImageTime = str_extract(`Original Image Name`, "_\\d{2}\\.\\d{2}\\.\\d{2}_") %>% str_remove_all("_")
  )

# === Filter to DP data only ===
df_filtered <- df %>%
  filter(`Surpass Object` %in% c("PV DP Soma", "PV DP Process")) %>%
  mutate(
    Compartment = case_when(
      `Surpass Object` == "PV DP Soma" ~ "Soma",
      `Surpass Object` == "PV DP Process" ~ "Process"
    )
  )

# === Count DPs per image per compartment ===
DP_counts <- df_filtered %>%
  group_by(Animal, ImageTime, Compartment) %>%
  summarise(DP_Count = n(), .groups = "drop")

# === Exclude (0,0) and (only soma, no process) at image level ===
DP_image_wide <- DP_counts %>%
  pivot_wider(names_from = Compartment, values_from = DP_Count, values_fill = 0) %>%
  filter(!(Process == 0 & Soma == 0)) %>%
  filter(!(Process == 0 & Soma > 0))

# === Compute mean DPs per animal per compartment ===
DP_counts_filtered <- DP_image_wide %>%
  pivot_longer(cols = c(Process, Soma), names_to = "Compartment", values_to = "DP_Count") %>%
  group_by(Animal, Compartment) %>%
  summarise(Mean_DP_Count = mean(DP_Count), .groups = "drop") %>%
  mutate(Group = factor(Compartment, levels = c("Process", "Soma"))) %>%
  rename(Value = Mean_DP_Count)

# === Plot Colors ===
group_colors <- c("Process" = "#b3bbd2", "Soma" = "#235476")

# === Wide format for t-test ===
DP_wide_filtered <- DP_counts_filtered %>%
  select(Animal, Group, Value) %>%
  pivot_wider(names_from = Group, values_from = Value)

# === Long format for plotting ===
DP_counts_filtered_long <- DP_wide_filtered %>%
  pivot_longer(cols = c(Process, Soma), names_to = "Group", values_to = "Value") %>%
  mutate(Group = factor(Group, levels = c("Process", "Soma")))

# === Set label position ===
label_y <- max(DP_counts_filtered_long$Value, na.rm = TRUE) + 1

# === Final Plot ===
p <- ggplot(DP_counts_filtered_long, aes(x = Group, y = Value, fill = Group)) +
  geom_violin(trim = FALSE, scale = "width", color = "black") +
  geom_boxplot(width = 0.15, color = "black", fill = "white", outlier.shape = NA) +
  geom_line(aes(group = Animal), color = "#43909d", linewidth = 1) +
  geom_point(size = 3, shape = 21, stroke = 1, color = "black", fill = "white", position = position_dodge(width = 0)) +
  stat_compare_means(
    method = "t.test",
    label = "p.signif",
    comparisons = list(c("Process", "Soma")),
    label.y = label_y,
    tip.length = 0.01,
    paired = TRUE
  ) +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(
    name = "Mean Number of DPs per Animal (Filtered)",
    breaks = seq(0, ceiling(max(DP_counts_filtered_long$Value, na.rm = TRUE)), by = 2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  xlab("") +
  theme_minimal(base_size = 14) +
  theme(
    axis.line.y = element_line(size = 0.8, color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.line.x = element_line(size = 0.8, color = "black"),
    axis.ticks.x = element_blank(),
    axis.ticks.length = unit(0.3, "cm"),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_blank(),
    legend.position = "none",
    axis.line = element_line(size = 1.2, colour = "black")
  )

# === Save t-test results ===
save_t_test_results(
  group1 = DP_wide_filtered$Process,
  group2 = DP_wide_filtered$Soma,
  comparison_label = "DP Process vs Soma (filtered, no 0-0 and no soma-only)",
  paired = TRUE,
  output_file = "Mean Number of DPs per animal_filtered_no00_nosomaonly.csv"
)

# === Save Plot ===
ggsave("DP_count_mean_per_animal_filtered_no00_nosomaonly.png", plot = p, width = 6, height = 6, dpi = 300)
print(p)

