library(lmtest)
library(urca)

# Load Dataset
data(ChickEgg, package = "lmtest")


################################################################################
#                   QUESTION 1: Order of integration for ChickEgg              #
################################################################################

# Assign variables
chicken <- ChickEgg[, "chicken"]
egg <- ChickEgg[, "egg"]



# Create function for extracting values and printing output
adf_test <- function(variable, diff = 1) {
    # Take tau statistic
    adf_output <- ur.df(variable, lag = diff)

    # Create summary for ADF-test
    adf_summary <- summary(adf_output)

    # Extract tau Value
    tau_value <- adf_summary@teststat[, "tau1"]

    # Extract critical Value
    cval_5pct <- adf_summary@cval[, 2]

    cat(
        "Tau-value for variable",
        substitute(variable),
        "of difference",
        diff, "is", tau_value,
        "and critical value of 5% significance level is",
        cval_5pct
    )

    if (tau_value < cval_5pct) {
        cat("\nReject NULL (unit root) at difference of", diff, "\n\n")
    } else {
        cat("\nDo not reject NULL (unit root) at difference of", diff, "\n\n")
    }
}

adf_test(chicken, diff = 1)
adf_test(chicken, diff = 2)
adf_test(chicken, diff = 3)

adf_test(egg, diff = 1)
adf_test(egg, diff = 2)
adf_test(egg, diff = 3)
