# Load required packages
library(openxlsx)
library(ggplot2)
library(ggridges)

# Load the data
df <- read.xlsx("/Users/champduan/Desktop/工作簿3.xlsx", sheet = 1)

# Set the factor levels for Model to adjust the order
df$Model <- factor(df$Model, levels = c("DeepSeek R1", "GPT-3.5", "GPT-4", "GPT-4o"))

# Box Plot
p <- ggplot(df, aes(x = Model, y = Score, fill = Model)) +
  geom_boxplot() +
  labs(x = "Model",
       y = "5-Likert Scale") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral", "lightgoldenrodyellow"))

# Save the box plot as a PNG file
ggsave("Exploratory analysis Boxplot.pdf", p, width = 7, height = 5)

# 2. Ridge Plot - Visualizing the density of 5-Likert Scale scores for each model
p <- ggplot(df, aes(x = Score, y = Model, fill = Model)) +
  geom_density_ridges(scale = 1.2, alpha = 0.8, rel_min_height = 0.01) +
  labs(x = "5-Likert Scale", y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")                       # Remove legend for clarity

# Save the ridge plot as a PNG file
ggsave("Exploratory analysis Ridge plot.pdf", plot = p, width = 8, height = 6)