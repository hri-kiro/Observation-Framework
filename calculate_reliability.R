# Install necessary packages (run only once)
#install.packages("irr")
#install.packages("psych")
#install.packages("dplyr")

# Load necessary libraries
library(irr)
library(psych)
library(dplyr)

# Set the path to your CSV file
csv_file_path <- "C:\\Rdata\\coders_data.csv"

# Load the CSV file
data <- read.csv(csv_file_path)

# Map string values to nominal values
data <- data %>%
  mutate(across(everything(), ~ case_when(
    . == "Approach" ~ 0,
    . == "Pass" ~ 1,
    . == "Avoid" ~ 2,
    . == "Follow" ~ 3,
    . == "Touch" ~ 4,
    . == "Gesture" ~ 5,
    . == "None" ~ 6,
    TRUE ~ NA_integer_  # Handles any other values as NA
  )))

# Remove rows with NA values
data <- na.omit(data)

# Display the modified data
head(data)

# Extract coder ratings from the CSV
# Assuming the CSV file has two columns: coder1 and coder2
coder1 <- data[, 1]
coder2 <- data[, 2]

# Function to calculate Scott's Pi manually
calculate_scotts_pi <- function(coder1, coder2) {
  # Create a contingency table of coder agreements
  table_data <- table(coder1, coder2)
  
  # Total number of ratings
  total_cases <- sum(table_data)
  
  # Observed agreement
  observed_agreement <- sum(diag(table_data)) / total_cases
  
  # Expected agreement
  row_marginals <- rowSums(table_data)
  col_marginals <- colSums(table_data)
  expected_agreement <- sum((row_marginals * col_marginals) / total_cases^2)
  
  # Scott's Pi calculation
  scotts_pi <- (observed_agreement - expected_agreement) / (1 - expected_agreement)
  
  return(scotts_pi)}

# Function to calculate Krippendorff's Alpha manually for nominal data
calculate_kripp_alpha_nominal <- function(coder1, coder2) {
  
  # Create a contingency table of coder agreements
  table_data <- table(coder1, coder2)
  
  # Number of cases
  num_cases <- sum(table_data)
  
  # Calculate observed disagreement (Do)
  observed_disagreement <- 0
  for (i in 1:nrow(table_data)) {
    for (j in 1:ncol(table_data)) {
      if (i != j) {
        observed_disagreement <- observed_disagreement + table_data[i, j]
      }
    }
  }
  Do <- observed_disagreement / num_cases
  
  # Calculate expected disagreement (De)
  row_marginals <- rowSums(table_data)
  col_marginals <- colSums(table_data)
  
  expected_disagreement <- 0
  for (i in 1:length(row_marginals)) {
    for (j in 1:length(col_marginals)) {
      if (i != j) {
        expected_disagreement <- expected_disagreement + (row_marginals[i] * col_marginals[j])
      }
    }
  }
  De <- expected_disagreement / (num_cases * num_cases)
  
  # Krippendorff's Alpha
  kripp_alpha <- 1 - (Do / De)
  
  return(kripp_alpha)
}

# Function to calculate agreement metrics
calculate_agreement <- function(coder1, coder2) {
  
  # Create a contingency table of the coders' decisions
  table_data <- table(coder1, coder2)
  
  # Number of cases and number of decisions
  num_cases <- length(coder1)
  num_decisions <- num_cases * 2
 
  # Number of agreements and disagreements
  num_agreements <- sum(diag(table_data))
  num_disagreements <- num_cases - num_agreements
  
  # Percent agreement
  percent_agreement <- num_agreements / num_cases * 100
  
  # Cohen's Kappa (no weights argument needed for unweighted nominal data)
  kappa_value <- cohen.kappa(data.frame(coder1, coder2))$kappa
    
  # Calculate Scott's Pi manually
  scotts_pi <- calculate_scotts_pi(coder1, coder2)

  # Calculate Krippendorff's Alpha manually (nominal)
  kripp_alpha <- calculate_kripp_alpha_nominal(coder1, coder2)

  # Return the results
  list(
    "Percent Agreement" = percent_agreement,
    "Scott's Pi" = scotts_pi,
    "Cohen's Kappa" = kappa_value,
    "Krippendorff's Alpha (Nominal)" = kripp_alpha,
    "Number of Agreements" = num_agreements,
    "Number of Disagreements" = num_disagreements,
    "Number of Cases" = num_cases,
    "Number of Decisions" = num_decisions
  )}

# Calculate agreement metrics
agreement_results <- calculate_agreement(coder1, coder2)

# Display results
print(agreement_results)