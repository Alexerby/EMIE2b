library(lmtest)
library(tseries)

data(ChickEgg, package = "lmtest")



################################################################################
#                   QUESTION 1: Order of integration for ChickEgg              #
################################################################################

# Checking order of integration for chicken variable in dataset ChickEgg

# Choose variable
chicken <- ChickEgg[, "chicken"]

# Initialize necessary variables
significance_level <- 0.05
difference <- 0
p_value <- 1

# While the p_value is greater than significance level keep looping
while (p_value >= significance_level) {
    difference <- difference + 1 # increment the difference by 1

    # Check difference against dataset to not take a difference greater than the length of the dataset
    if (difference > length(chicken) - 1) {
        break
    }
    chicken_diff <- diff(chicken, differences = difference) # otherwise take difference of order d
    adf_test <- adf.test(chicken_diff) # perform adf test
    p_value <- adf_test$p.value # assign the p_value to variable

    if (p_value < significance_level) {
        break
    } else {
        print(paste("ADF test for", difference, "order difference: p-value =", p_value))
    }
}

cat("We need to difference the time series data of variable 'chicken'", difference, "time(s)")
