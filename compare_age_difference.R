# Load necessary library
library(dplyr)

# Load data
data <- read.csv("C:\\Rdata\\compare_difference.csv")

# Summarize mean and standard deviation for each Age category
summary_stats <- data %>%
  group_by(Age) %>%
  summarise(
    Obs_prox_mean = mean(Obs_prox, na.rm = TRUE),
    Obs_prox_sd = sd(Obs_prox, na.rm = TRUE),
    Obs_int_mean = mean(Obs_int, na.rm = TRUE),
    Obs_int_sd = sd(Obs_int, na.rm = TRUE),
    Freq_prox_mean = mean(Freq_prox, na.rm = TRUE),
    Freq_prox_sd = sd(Freq_prox, na.rm = TRUE),
    Freq_int_mean = mean(Freq_int, na.rm = TRUE),
    Freq_int_sd = sd(Freq_int, na.rm = TRUE)
  )

print(summary_stats)

# Mann-Whitney U tests for each variable between age groups
obs_prox_test <- wilcox.test(Obs_prox ~ Age, data = data)
obs_int_test <- wilcox.test(Obs_int ~ Age, data = data)
freq_prox_test <- wilcox.test(Freq_prox ~ Age, data = data)
freq_int_test <- wilcox.test(Freq_int ~ Age, data = data)

# Display U-value and p-value for each test
cat("Obs_prox U-value:", obs_prox_test$statistic, "p-value:", obs_prox_test$p.value, "\n")
cat("Obs_int U-value:", obs_int_test$statistic, "p-value:", obs_int_test$p.value, "\n")
cat("Freq_prox U-value:", freq_prox_test$statistic, "p-value:", freq_prox_test$p.value, "\n")
cat("Freq_int U-value:", freq_int_test$statistic, "p-value:", freq_int_test$p.value, "\n")