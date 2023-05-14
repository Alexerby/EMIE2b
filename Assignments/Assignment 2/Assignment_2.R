library(lmtest)
library(tseries)

data(ChickEgg, package = "lmtest")



################################################################################
#                   QUESTION 1: Order of integration for ChickEgg              #
################################################################################


# Define function for checking the required order of integration using the augmented dickey fuller test

order_of_integration <- function(variable, significance_level = 0.05, p_value = 1, difference = 0) {
    while (p_value >= significance_level) {
        difference <- difference + 1 # increment the difference by 1

        # Check difference against dataset to not take a difference greater than the length of the dataset
        if (difference > length(variable) - 1) {
            break
        }
        chicken_diff <- diff(variable, differences = difference) # otherwise take difference of order d
        adf_test <- suppressWarnings(adf.test(chicken_diff)) # perform adf test
        p_value <- adf_test$p.value # assign the p_value to variable

        if (p_value < significance_level) {
            cat("We need to difference the time series data of variable", substitute(variable), difference, "time(s) \n\n")
            break
        } else {
            print(paste("ADF test for", difference, "order difference: p-value =", p_value))
        }
    }
}

chicken <- ChickEgg[, "chicken"]
egg <- ChickEgg[, "egg"]

# Checking order of integration for chicken and egg variable using our defined function
order_of_integration(chicken)
order_of_integration(egg)
