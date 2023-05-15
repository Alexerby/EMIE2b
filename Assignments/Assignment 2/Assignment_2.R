library(lmtest)
library(tseries)
library(TS)
library(urca)

data(ChickEgg, package = "lmtest")



################################################################################
#                   QUESTION 1: Order of integration for ChickEgg              #
################################################################################


# Define function for checking the required
# order of integration using the augmented dickey fuller test
find_integration_order <- function(variable,
                                   significance_level = 0.05,
                                   p_value = 1,
                                   difference = 0) {
    while (p_value >= significance_level) {
        difference <- difference + 1 # increment the difference by 1

        # Check difference against dataset to not
        # take a difference greater than the length of the dataset
        if (difference > length(variable) - 1) {
            break
        }

        # otherwise take difference of order d
        chicken_diff <- diff(variable, differences = difference)
        adf_test <- suppressWarnings(adf.test(chicken_diff)) # perform adf test
        p_value <- adf_test$p.value # assign the p_value to variable

        if (p_value < significance_level) {
            cat(
                "The order of integration for variable",
                substitute(variable), "at the significance level of",
                significance_level, "is",
                difference,
                "\n\n"
            )
            break
        } else {
            print(paste(
                "ADF test for",
                difference,
                "order difference: p-value =",
                p_value
            ))
        }
    }
}

# Define the variables we want to test
chicken <- ChickEgg[, "chicken"]
egg <- ChickEgg[, "egg"]

# Call find_integration_order()
find_integration_order(chicken)
find_integration_order(egg)




# However its not necessarily as easy as comparing the
# p-value to the significance level, we need to consider
# multiple methods and statistics when deciding on the
# order of integration. Another important way of
# making a decision is by comparing the tau-value
# of the DF-test to the critical value. We will do
# this by using the modified urca.df() function
# defined by prof. Michael Lundholm

ADF.test(chicken)
ADF.test(egg)

# We can see that his function will choose the same lags
# as our function did, i.e. a lag of 1 for egg and
# a lag of 2 for chicken. It is also worth considering
#

significance_level <- 0.05

ur.df(chicken)

ADF.test(chicken)

test_statistic <- ur.df(chicken)@teststat[, "tau1"]
critical_value <- ur.df(chicken)@cval[, "5pct"]
