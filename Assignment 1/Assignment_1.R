# Load required libraries
library(Ecdat)
library(dynlm)
library(TS)


# Load dataset IncomeUK (from package Ecdat)
data(IncomeUK, package = "Ecdat")

################################################################################
#                                   QUESTION 1                                 #
################################################################################

# Initialize empty list for the models
model_list <- list()

for (i in 1:5) {
    # Fit the model with the current lag value
    model <- dynlm(income ~ L(income, 1:i), data = IncomeUK)

    # Add the model object to the list
    model_list[[i]] <- model

    # Assign the model object to a variable
    # with a name that follows the pattern "model_i"
    assign(paste0("model_", i), model_list[[i]])
}

################################################################################
#                                   QUESTION 2                                 #
################################################################################

# Calculating the AIC for the different models
aic_values <- c(
    AIC(model_1),
    AIC(model_2),
    AIC(model_3),
    AIC(model_4),
    AIC(model_5)
)

# Determine the minimum value of the models
best_model_aic <- which.min(aic_values)
cat("Best model based on AIC: ", best_model_aic, "\n")

################################################################################
#                                   QUESTION 3                                 #
################################################################################



#########################################################
#                   Ljung-Box test                      #
#########################################################

for (i in 1:length(model_list)) {
    # Create variables for each models residuals
    residuals_model <- paste0("residuals_model_", i)
    residuals <- residuals(model_list[[i]])
    assign(residuals_model, residuals)

    # Perform Ljung-Box test on residuals
    cat("\n Ljung-Box output for model", i, ":\n")
    print(TS::LjungBox(get(residuals_model)))
}

# Assign variable names for ljung-box test called lb_test1,...,lb_test5
for (i in 1:5) {
    lb_test <- TS::LjungBox(get(paste0("residuals_model_", i)))
    assign(paste0("lb_test", i), lb_test)
}

lb_test

# Compare p-value with a significance level of 5 percent for our models
for (i in 1:5) {
    cat("\nP-values comparing for model", i, ":\n")
    lb_test <- get(paste0("lb_test", i))

    for (j in 1:ncol(lb_test)) {
        if (lb_test[j, "p-value"] > 0.05) {
            cat("Lag", lb_test[j, "lags"], "p-value > \u03B1 (0.05) \u27f6 do not reject NULL hypothesis\n")
        } else {
            cat("Lag", lb_test[j, "lags"], "has p-value \u2264 \u03B1 (0.05) \u27f6 reject NULL hypothesis.\n")
        }
    }
}
# We can see from the output of the above that we can not reject
# H_0 when it comes to income_{t-5}

#########################################################
#                   Jarque-Bera test                    #
#########################################################
