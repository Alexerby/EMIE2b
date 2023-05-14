library(lmtest)
library(tseries)

data(ChickEgg, package = "lmtest")



################################################################################
#                   QUESTION 1: Order of integration for ChickEgg              #
################################################################################


order_of_integration <- function(variable) {
    while (p_value >= significance_level) {
        difference <- difference + 1 # increment the difference by 1

        # Check difference against dataset to not take a difference greater than the length of the dataset
        if (difference > length(variable) - 1) {
            break
        }
        chicken_diff <- diff(variable, differences = difference) # otherwise take difference of order d
        adf_test <- adf.test(chicken_diff) # perform adf test
        p_value <- adf_test$p.value # assign the p_value to variable

        if (p_value < significance_level) {
            cat("We need to difference the time series data of variable 'chicken'", difference, "time(s)")
            break
        } else {
            print(paste("ADF test for", difference, "order difference: p-value =", p_value))
        }
    }
}



# Checking order of integration for chicken variable in dataset ChickEgg

# Choose variable
chicken <- ChickEgg[, "chicken"]

# Initialize necessary variables
significance_level <- 0.05
difference <- 0
p_value <- 1

order_of_integration(chicken)
