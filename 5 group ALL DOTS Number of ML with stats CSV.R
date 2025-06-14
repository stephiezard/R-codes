# === Load Libraries ===
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(ggpubr)

# === Load Data ===
df <- read_excel("volume_5.xlsx", col_names = TRUE, skip = 1)

# === Reusable Function: Paired or Unpaired T-Test Export ===
save_t_test_results <- function(group1, group2, comparison_label = "Comparison", paired = TRUE, output_file = "t_test_results.csv") {
  # Run t-test
  test_res <- t.test(group1, group2, paired = paired)
  
  # Compute values
  mean1 <- mean(group1, na.rm = TRUE)
  mean2 <- mean(group2, na.rm = TRUE)
  diff_mean <- mean(group1 - group2, na.rm = TRUE)
  sem_diff <- sd(group1 - group2, na.rm = TRUE) / sqrt(length(group1))
  
  # Calculate eta squared
  t_val <- as.numeric(test_res$statistic)
  df_val <- as.numeric(test_res$parameter)
  eta_sq <- t_val^2 / (t_val^2 + df_val)
  
  # Format results into a tibble
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
  
  # Save to CSV
  readr::write_csv(result, output_file)
  return(result)
}

# === Extract Animal and Image Time ===
df <- df %>%
  mutate(
    Animal = str_extract(`Original Image Name`, "SL_16_(\\d+)") %>% str_remove("SL_16_"),
    ImageTime = str_extract(`Original Image Name`, "_\\d{2}\\.\\d{2}\\.\\d{2}_") %>% str_remove_all("_")
  )

# === Filter to ML data only ===
df_filtered <- df %>%
  filter(`Surpass Object` %in% c("PV ML Soma", "PV ML Process")) %>%
  mutate(
    Compartment = case_when(
      `Surpass Object` == "PV ML Soma" ~ "Soma",
      `Surpass Object` == "PV ML Process" ~ "Process"
    )
  )

# === Count MLs per image per compartment ===
ml_counts <- df_filtered %>%
  group_by(Animal, ImageTime, Compartment) %>%
  summarise(ML_Count = n(), .groups = "drop")

# === Pivot Wider: Process vs Soma Count ===
ml_wide <- ml_counts %>%
  pivot_wider(names_from = Compartment, values_from = ML_Count, values_fill = 0) %>%
  mutate(
    Ratio = Process / (Soma + 1e-6),  # avoid divide-by-zero
    GroupAnimal = Animal
  )

# === Prepare Long Format for Plot ===
ml_long <- ml_counts %>%
  mutate(
    GroupAnimal = paste(Animal, Compartment),
    Group = factor(Compartment, levels = c("Process", "Soma"))
  ) %>%
  rename(Value = ML_Count)

# === Create per-animal comparison list ===
comparisons <- lapply(unique(ml_long$Animal), function(animal) {
  c(paste(animal, "Process"), paste(animal, "Soma"))
})

# === Plot Colors ===
group_colors <- c("Process" = "#43909d", "Soma" = "#e2cde8")

# Define y-axis ticks
y_breaks <- pretty(ml_counts$Value, n = 5)

# Set label position just above data
label_y <- max(ml_counts$Value, na.rm = TRUE) + 1

# === Final Plot ===
p <- ggplot(ml_long, aes(x = Group, y = Value, fill = Group)) +
  geom_violin(trim = FALSE, scale = "width", color = "black") +
  geom_boxplot(width = 0.15, color = "black", fill = "white", outlier.shape = NA) +
  geom_jitter(size = 2.2, shape = 21, stroke = 1, color = "black", fill = "white", width = 0.15) +
  stat_compare_means(
    comparisons = list(c("Process", "Soma")),
    method = "t.test",
    label = "p.signif",
    tip.length = 0.01
  ) +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(
    name = "Total Number of MLs in Compartments",
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

#T-test CSV
save_t_test_results(
  group1 = ml_wide$Process,
  group2 = ml_wide$Soma,
  comparison_label = "DP Process vs Soma",
  paired = TRUE,
  output_file = "Total Number of MLs in Compartments.csv"
)

# === Save Plot ===
ggsave("ML_count_Soma_vs_Process_clean_style.png", plot = p, width = 6, height = 5, dpi = 300)
print(p)

