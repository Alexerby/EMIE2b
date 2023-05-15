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
extract_values <- function(variable, diff = 1) {
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
        cval_5pct, "\n\n"
    )
}

extract_values(chicken, diff = 1)
extract_values(chicken, diff = 2)
extract_values(chicken, diff = 3)

extract_values(egg, diff = 1)
extract_values(egg, diff = 2)
extract_values(egg, diff = 3)
