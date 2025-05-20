
# Cumulative plot ---------------------------------------------------------

# Load required libraries
library(ggplot2)
library(openxlsx)
library(dplyr)
library(tidyr)

# Load the data
input <- read.xlsx('Supplement 1.xlsx', sheet = 1)

# Select relevant columns: ID, evaluator, and Likert scores from four LLMs
input <- input[, c(10, 14, 18, 22)]
names(input)[1:4] <- c("GPT-3.5", "GPT-4", "Claude 3 Sonnet", "Claude 3 Opus")

# Reshape to long format and compute cumulative frequencies
data_long <- input %>%
  gather(key = "Model", value = "Likert_Score", starts_with("GPT"), starts_with("CLAUDE")) %>%
  group_by(Model, Likert_Score) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  group_by(Model) %>%
  arrange(as.numeric(Likert_Score)) %>%
  mutate(Cumulative_Frequency = cumsum(Frequency) / sum(Frequency) * 100)

# Define color palette for models
color_palette <- c("#D55E00", "#E69F00", "#56B4E9", "#009E73")

# Create cumulative frequency step plot
p <- ggplot(data_long, aes(x = as.numeric(Likert_Score), y = Cumulative_Frequency, color = Model)) +
  geom_step(size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(
    values = color_palette,
    breaks = c("GPT-3.5", "GPT-4", "Claude 3 Sonnet", "Claude 3 Opus")
  ) +
  scale_x_continuous(breaks = 1:5, limits = c(1, 5)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = "Likert Scale",
    y = "Cumulative Frequency [%]",
    title = "Cumulative Frequency of Accuracy Evaluations"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    panel.grid.major = element_line(color = "gray70", size = 0.5, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)
  )

# Save the plot in both PNG and PDF formats
ggsave("Cumplot.pdf", plot = p, width = 8, height = 8)



# Bubble plot -------------------------------------------------------------

# Load required libraries
library(ggplot2)
library(dplyr)
library(openxlsx)
library(patchwork)

# Load the data
input <- read.xlsx('Supplement 1.xlsx', sheet = 1)
input <- input[, c(10, 14, 18, 22)] 
names(input)[1:4] <- c("GPT_3.5", "GPT_4", "Claude3_Sonnet", "Claude3_Opus")

# Compute frequency table
d_gpt <- as.data.frame(table(input[, c("GPT_3.5", "GPT_4")]))
names(d_gpt) <- c("GPT_3.5", "GPT_4", "Freq")

# Ensure values are numeric and within valid range
d_gpt$GPT_3.5 <- as.numeric(as.character(d_gpt$GPT_3.5))
d_gpt$GPT_4 <- as.numeric(as.character(d_gpt$GPT_4))
d_gpt <- d_gpt[d_gpt$Freq > 0, ]

# Paired Wilcoxon test (one-sided: GPT-3.5 < GPT-4)
p_value_gpt <- wilcox.test(input$GPT_3.5, input$GPT_4, paired = TRUE, alternative = "less")$p.value
p_value_gpt <- formatC(p_value_gpt * 6, format = "e", digits = 2)  # Bonferroni correction for 6 tests

# Bubble plot
p1 <- ggplot(d_gpt, aes(x = GPT_3.5, y = GPT_4)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50", size = 1) +  # Reference diagonal
  geom_point(aes(size = Freq), fill = "#F08A5D", alpha = 0.85, shape = 21, color = "black") +
  labs(title = "", x = "GPT-3.5", y = "GPT-4") +
  annotate("text", x = 4.9, y = 1.1, label = paste0("p = ", p_value_gpt), size = 6,
           hjust = 1, vjust = 0, fontface = "italic", color = "black") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    axis.text = element_text(size = 18),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    axis.line = element_blank()
  ) +
  scale_x_continuous(breaks = 1:5, limits = c(1, 5)) +
  scale_y_continuous(breaks = 1:5, limits = c(1, 5))

ggsave('3.5_vs_4_bubble_plot.pdf', p1, width = 5, height = 5)

## Repeat similar procedure for the following model comparisons:
## 1. CLAUDE3-Sonnet vs CLAUDE3-Opus → save as 'sn_vs_op_bubble_plot.pdf'
## 2. CLAUDE3-Sonnet vs GPT-4        → save as 'sn_vs_gpt4_bubble_plot.pdf'
## 3. CLAUDE3-Opus vs GPT-4          → save as 'op_vs_gpt4_bubble_plot.pdf'
##    For each: compute frequency table, Wilcoxon test, and plot.


