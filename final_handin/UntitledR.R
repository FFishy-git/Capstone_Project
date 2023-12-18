# Load necessary libraries
library(readr)
library(dplyr)

# Load the data
data <- read_csv("data/cleaned/IPEDS_with_scores.csv")
# add a column for index
data$index <- 1:nrow(data)
# Load data for regression
data_not_missing <- read_csv("data/cleaned/IPEDS_for_regression.csv")

# Define the columns to fill
score_name_list <- c(
  'SAT Critical Reading 25th percentile score',
  'SAT Math 25th percentile score',
  'SAT Writing 25th percentile score',
  'SAT Critical Reading 75th percentile score',
  'SAT Math 75th percentile score',
  'SAT Writing 75th percentile score',
  'ACT Composite 25th percentile score',
  'ACT Composite 75th percentile score'
)

for(i in 1:255) {
  missingness_pattern <- as.integer(intToBits(i)[1:8])
  data_missing <- data
  missing_columns <- c()
  nonmissing_columns <- c()
  for(j in 1:8) {
    if(missingness_pattern[j] == 1) {
      data_missing <- data_missing[is.na(data_missing[[score_name_list[j]]]),]
      missing_columns <- c(missing_columns, score_name_list[j])
    } else {
      data_missing <- data_missing[!is.na(data_missing[[score_name_list[j]]]),]
      nonmissing_columns <- c(nonmissing_columns, score_name_list[j])
    }
  }
  # if there is no missingness, we skip
  if(nrow(data_missing) == 0) next
  # if there is missingness, we run linear regression on the non-missing rows to predict the missing ones
  X <- data_not_missing[, nonmissing_columns]
  y <- data_not_missing[, missing_columns]
  
  # create an empty dataframe y_pred to store the predicted values, where the column names are given by missing_columns
  y_pred <- data.frame(matrix(ncol = length(missing_columns), nrow = nrow(data_missing)))
  colnames(y_pred) <- missing_columns
  for (column in colnames(y)) {
    reg <- train(X, y[[column]], method = "lm")
    y_pred[, column] <- predict(reg, newdata = data_missing[, nonmissing_columns])
  }
  
  # Store y_pred into data at the correct rows and columns
  data[data_missing$index, missing_columns] <- y_pred
}

# Store the data
write_csv(data, "data/cleaned/IPEDS_filled.csv")