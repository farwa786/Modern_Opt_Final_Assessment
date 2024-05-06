# Continued R script to print accuracy and fitness tables

# Function to compute descriptive statistics and format them into a table
computeStatsTable <- function(data) {
  mean_val <- mean(data$best_mean)
  sd_val <- sd(data$best_mean)
  error_margin <- qt(0.975, df=length(data$best_mean)-1) * sd_val / sqrt(length(data$best_mean))
  
  data.frame(
    Mean = mean_val,
    SD = sd_val,
    Lower_CI = mean_val - error_margin,
    Upper_CI = mean_val + error_margin
  )
}

# Calculate and print tables for each population size
stats_table15 <- computeStatsTable(parsed_data1)
stats_table20 <- computeStatsTable(parsed_data2)
stats_table30 <- computeStatsTable(parsed_data3)

# Print the tables
cat("Statistics Table for Population Size 15:\n")
print(stats_table15)
cat("\nStatistics Table for Population Size 20:\n")
print(stats_table20)
cat("\nStatistics Table for Population Size 30:\n")
print(stats_table30)
