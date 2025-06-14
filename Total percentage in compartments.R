# === Load Libraries ===
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tibble)
library(readr)

# === Load Data ===
df <- read_excel("volume_5.xlsx", col_names = TRUE, skip = 1)

# === Filter for 4 relevant object categories ===
df_filtered <- df %>%
  filter(`Surpass Object` %in% c("PV ML Process", "PV ML Soma", "PV DP Process", "PV DP Soma")) %>%
  mutate(
    Category = case_when(
      `Surpass Object` == "PV ML Process" ~ "ML Process",
      `Surpass Object` == "PV ML Soma" ~ "ML Soma",
      `Surpass Object` == "PV DP Process" ~ "DP Process",
      `Surpass Object` == "PV DP Soma" ~ "DP Soma"
    )
  )

# === Count number of objects per group ===
object_counts <- df_filtered %>%
  count(Category, name = "Count") %>%
  mutate(
    Total = sum(Count),
    Proportion = Count / Total,
    Percent = Proportion * 100,
    SE_percent = sqrt(Proportion * (1 - Proportion) / Total) * 100,
    ymin = Percent - SE_percent,
    ymax = Percent + SE_percent
  )

# === Bar Colors ===
group_colors <- c(
  "DP Process" = "#b3bbd2",
  "DP Soma" = "#235476",
  "ML Process" = "#43909d",
  "ML Soma" = "#e2cde8"
)

# === Plot ===
p <- ggplot(object_counts, aes(x = Category, y = Percent, fill = Category)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    width = 0.1,
    size = 0.4
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", Percent)),
    vjust = -0.5,
    size = 5
  ) +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.05))
  ) +
  ylab("All identified puncta's distribution") +
  xlab("") +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(size = 1.2, colour = "black"),
    axis.line.y = element_line(size = 1.2, colour = "black"),         # y-axis line
    axis.ticks.y = element_line(size = 0.8, color = "black"),         # y-axis ticks
    axis.ticks.length = unit(0.3, "cm"),                              # tick length
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    panel.grid = element_blank(),
    legend.position = "none"
  )


ggsave("Total_Percentage_4groups_DP_ML.svg", plot = p, width = 6, height = 5, dpi = 300)
print(p)

# === Chi-squared test of independence ===
chisq_test <- chisq.test(object_counts$Count)

# === Save result summary ===
results <- tibble(
  Groups = "DP Process vs DP Soma vs ML Process vs ML Soma",
  `Chi-squared` = round(chisq_test$statistic, 3),
  `df` = chisq_test$parameter,
  `p-value` = signif(chisq_test$p.value, 4)
)

# === Save CSVs ===
write_csv(object_counts, "4group_percentages_DP_vs_ML.csv")
write_csv(results, "4group_chisq_test_DP_vs_ML.csv")
print(results)
