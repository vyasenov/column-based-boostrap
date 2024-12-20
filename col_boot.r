rm(list=ls())
set.seed(1988)

##############
############## GENERATE DATA
##############

data <- as.data.frame(matrix(rnorm(1000), nrow = 50, ncol = 20))

# Specify the two variables to test correlation
var1 <- 1  # Index of the first variable
var2 <- 2  # Index of the second variable

# Compute the observed correlation
observed_correlation <- cor(data[[var1]], data[[var2]])

##############
############## RUN BOOTSTRAP
##############

b <- 1000  # Number of bootstrap iterations
bootstrap_correlations <- numeric(b) # Vector to store bootstrap correlations

for (i in 1:b) {
  resampled_columns <- sample(1:ncol(data), size = 2, replace = FALSE)
  bootstrap_correlations[i] <- cor(data[resampled_columns[1]], data[resampled_columns[2]])
}

# Test the significance of the observed correlation
p_value <- mean(abs(bootstrap_correlations) >= abs(observed_correlation))

##############
############## PRINT RESULTS
##############

cat("Observed Correlation:", observed_correlation, "\n")
cat("P-value:", p_value, "\n")

##############
############## PLOT THE BOOTSTRAP DISTRIBUTION
##############

hist(bootstrap_correlations, 
    breaks = 30, 
    main = "Bootstrap Correlation Distribution", 
    xlab = "Correlation Coefficient", 
    col = "lightblue")
abline(v = observed_correlation, 
    col = "red", 
    lwd = 2)
legend("topright", 
    legend = c("Observed Correlation"), 
    col = c("red"), 
    lwd = 2)