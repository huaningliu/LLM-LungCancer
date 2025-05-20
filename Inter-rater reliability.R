# Load required packages
library(cowplot)
library(boot)
library(openxlsx)
library(irr)
library(ggplot2)
library(reshape2)
library(dplyr)

# Load the data
# ratings_3.5 <- read.xlsx("inter rater reliability.xlsx", sheet = 1)
# ratings_4 <- read.xlsx("inter rater reliability.xlsx", sheet = 2)
# ratings_4o <- read.xlsx("inter rater reliability.xlsx", sheet = 3)
# ratings_ds <- read.xlsx("inter rater reliability.xlsx", sheet = 4)
# ratings_op <- read.xlsx("inter rater reliability.xlsx", sheet = 6)
ratings_sn <- read.xlsx("inter rater reliability.xlsx", sheet = 5)

#### Step 1: Define Kappa function ###
# Function to calculate weighted Kappa using the kappa2 function
kappa_function <- function(data, indices) {
  d <- data[indices, ]  # Resample data with bootstrap sampling
  result <- kappa2(d, weight = "squared")$value  # Calculate Kappa value
  return(result)
}

### Step 2: Calculate 95% Confidence Interval for Kappa using bootstrap ###
set.seed(123)  # Set seed for reproducibility
boot_result_sn <- boot(data = ratings_sn, statistic = kappa_function, R = 1000)  # Bootstrap for CLAUDE3-Sonnet
boot_ci_sn <- boot.ci(boot_result_sn, type = "perc")  # 95% CI

# Output results
cat("Kappa value (CLAUDE3-Sonnet):", boot_result_sn$t0, "\n")
cat("95% Confidence Interval:", boot_ci_sn$percent[4:5], "\n")

#### Step 3: Prepare Data for Bland-Altman Plot ###
# Calculate the mean and difference between the raters
ratings_sn$mean <- (ratings_sn$Rater1 + ratings_sn$Rater2) / 2
ratings_sn$diff <- ratings_sn$Rater1 - ratings_sn$Rater2

# Count the frequency of each (mean, diff) pair for the plot
data_count_sn <- ratings_sn %>%
  group_by(mean, diff) %>%
  summarise(count = n(), .groups = 'drop')

# Calculate Bland-Altman limits of agreement
mean_diff <- mean(ratings_sn$diff)
sd_diff <- sd(ratings_sn$diff)
upper_loa <- mean_diff + 1.96 * sd_diff  # Upper limit of agreement
lower_loa <- mean_diff - 1.96 * sd_diff  # Lower limit of agreement

#### Step 4: Generate Bland-Altman Plot ###
p <- ggplot(data_count_sn, aes(x = mean, y = diff, size = count)) +
  geom_point(alpha = 0.7, color = 'darkblue') +  # Scatter points
  geom_hline(yintercept = mean_diff, linetype = "dashed", color = "red") +  # Mean difference line
  geom_hline(yintercept = upper_loa, linetype = "dashed", color = "blue") +  # Upper limit line
  geom_hline(yintercept = lower_loa, linetype = "dashed", color = "blue") +  # Lower limit line
  labs(title = "Bland-Altman Plot for Rater Agreement (CLAUDE3-Sonnet)",
       x = "Mean of Ratings",
       y = "Difference (Rater1 - Rater2)") +
  theme_minimal()

# Save the plot as a PDF
ggsave("Bland Altman plot Claude 3 Sonnet.pdf", plot = p, width = 10, height = 8)