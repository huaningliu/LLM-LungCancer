
# Multicenters analysis ---------------------------------------------------

# Load required R packages
library(ggplot2)
library(dplyr)
library(ggpubr)
library(readxl)

# Read data from the 5th sheet of the Excel file
input <- read.xlsx(paste0("Supplement 1.xlsx"), sheet = 4)

# Calculate the mean and standard error of the mean (SEM) for each Model within each Center
data_summary <- input %>%
  group_by(Center, Model) %>%
  summarise(mean_score = mean(Score), sem_score = sd(Score) / sqrt(n()), .groups = "drop")

# Set the desired order of models for plotting
input$Model <- factor(input$Model, levels = c("GPT_3.5", "GPT_4", "Claude 3 Sonnet", "Claude 3 Opus"))

# Create violin plot with boxplot overlay and customized theme
p <- ggplot(input, aes(x = Model, y = Score, fill = Center)) +
  geom_violin(trim = FALSE, position = position_dodge(0.9), color = "black", alpha = 0.7) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9), alpha = 0.3, color = "black") +
  labs(title = "Multi-center Analysis",
       x = "",
       y = "5-Likert scale") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray80", size = 0.5),
    panel.grid.minor = element_line(color = "gray90", size = 0.25),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  scale_fill_brewer(palette = "Set2")

# Save the plot as a PDF
ggsave("multi-centers analysis.pdf", p, width = 14, height = 6)
