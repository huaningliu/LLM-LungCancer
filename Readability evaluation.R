# Readability evaluation --------------------------------------------------
# Load required libraries
library(ggplot2)
library(ggpubr)
library(openxlsx)
library(tidyverse)
library(dplyr)
library(rstatix)

# Load data from Excel (Sheet 2)
df <- read.xlsx('Supplement 1.xlsx', sheet = 2)

# Reshape data from wide to long format
df_long <- pivot_longer(df, cols = everything(), names_to = "Model", values_to = "FleschScore")

# Convert model names to a specific factor order
df_long$Model <- factor(df_long$Model, levels = c("GPT-3.5", "GPT-4", "Claude 3 Sonnet", "Cladue 3 Opus"))

# Calculate median Flesch scores for each model
medians <- sapply(df, median)
median_df <- data.frame(
  Model = names(medians),
  Median = round(medians, 2)
)
print(median_df)

# Plot boxplots of readability scores
p <- ggplot(df_long, aes(x = Model, y = FleschScore, fill = Model)) +
  geom_boxplot() +
  stat_summary(fun = median) +
  theme_minimal() +
  labs(title = "Flesch Readability Scores by Model",
       x = "Language Model",
       y = "Flesch Reading Ease Score") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))

# Save the plot as a PDF
ggsave('readability_evaluation.pdf', p, width = 10, height = 8)

# Perform pairwise Wilcoxon signed-rank tests with Bonferroni correction
pairwise_stats <- df_long %>%
  pairwise_wilcox_test(
    FleschScore ~ Model,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  )

# Display pairwise comparison results
print(pairwise_stats)