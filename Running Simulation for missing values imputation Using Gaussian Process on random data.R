#install.packages("missForest")
#install.packages("GauPro")

library(missForest)
library(GauPro)

set.seed(123)  # For reproducibility

# Simulated dataset generation function
generate_dataset <- function(n, p) {
  data <- matrix(rnorm(n * p), nrow = n, ncol = p)
  return(data)
}

# Gaussian process imputation function
impute_missing_values <- function(data) {
  imputed_data <- missForest(data)
  x <- as.data.frame(imputed_data$ximp)
  y <- as.matrix(data)
  gp_model <- GauPro(y , x)
  imputed_values <- gp_model$yhat
  return(imputed_values)
}
impute_missing_values

# Simulation loop
missing_rates <- c(5, 10, 20, 30, 40, 50)  # Missing value percentages
n_simulations <- 100  # Number of simulations

for (rate in missing_rates) {
  mae_results <- numeric(n_simulations)  # Store MAE results for each simulation
  
  for (i in 1:n_simulations) {
    # Generate dataset
    n <- 100
    p <- 5
    dataset <- generate_dataset(n, p)
    
    # Introduce missing values
    missing_indices <- sample(length(dataset), size = round(length(dataset) * rate/100))
    dataset[missing_indices] <- NA
    
    # Impute missing values using Gaussian process
    imputed_values <- impute_missing_values(dataset)
    
    # Compare imputed values to original values
    original_values <- dataset[missing_indices]
    imputation_error <- abs(original_values - imputed_values)
    
    # Calculate mean absolute error
    mae <- mean(imputation_error, na.rm = TRUE)
    mae_results[i] <- mae
  }
  
  # Print average MAE for the current missing rate
  avg_mae <- mean(mae_results)
  print(paste("Missing Rate:", rate, "% | Average MAE:", avg_mae))
}
