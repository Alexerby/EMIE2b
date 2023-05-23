# Load required libraries
library(Ecdat)
library(dynlm)
library(TS)
library(tseries) # used for jarque.bera.test

# Load dataset IncomeUK (from package Ecdat)
data(IncomeUK, package = "Ecdat")
income <- IncomeUK[, "income"] # Rename the variable to avoid conflict
income <- diff(income)

################################################################################
#                          QUESTION 1: Autoregressive models                    #
################################################################################

model_list <- list()

# Create lags
lags <- embed(income, 5)
lags <- lags[1:(length(income) - 5), ] # Keep only the relevant rows

for (i in 1:5) {
    assign(paste0("lag_", i), lags[, i])
}

for (lag in 1:5) {
    # Create the formula for the current lag
    formula <- as.formula(paste0("income[6:length(income)] ~ ", paste0("lag_", 1:lag, collapse = " + ")))

    # Fit the model with the current lag value
    model <- lm(formula, data = diff(IncomeUK))

    # Add the model object to the list
    model_list[[lag]] <- model
    assign(paste0("model_", lag), model_list[[lag]])
}

for (lag in 1:5) {
    print(model_list[[lag]])
    message(paste(rep("- ", 35), collapse = ""))
}


################################################################################
#                    QUESTION 2: Akaikes information criteria                  #
################################################################################

# Calculating the AIC for the different models
aic_values <- c(
    AIC(model_1), AIC(model_2), AIC(model_3), AIC(model_4), AIC(model_5)
)

# Determine the minimum value of the models
best_model_aic <- which.min(aic_values)
cat("Best model based on AIC: ", best_model_aic, "\n")

################################################################################
#                   QUESTION 3: Ljung-Box & Jarque-Bera tests                  #
################################################################################

# Define the model list
model_list <- list(model_1, model_2, model_3, model_4, model_5)

#########################################################
#                 3a) Ljung-Box test                    #
#########################################################

# For loop for our models
for (i in 1:length(model_list)) {
    # Create variables for each models residuals
    residuals_model <- paste0("residuals_model_", i) # residuals_model_i
    residuals <- residuals(model_list[[i]]) # store residuals in variable
    assign(residuals_model, residuals) # assign name for each
    residuals <- get(residuals_model)
    parameters <- (i + 1)
    output <- TS::LjungBox(residuals, lags = c(1:10), order = parameters) # save output of test
    cat("\nModel", i, ":\n")

    # Print p-value fo each row
    for (rows_index in 1:nrow(output)) {
        p_value <- output[rows_index, "p-value"]
        if (!is.na(p_value)) {
            cat("P-value of lag", rows_index, "is", round(p_value, 3))

            if (p_value < 0.05) {
                message("    (reject NULL hypothesis at 5% significance level)")
            } else {
                message("")
            }
        }
    }
}

#########################################################
#               3b) Jarque-Bera test                    #
#########################################################

for (i in 1:length(model_list)) {
    residuals <- residuals(model_list[[i]])
    output <- jarque.bera.test(residuals)
    p_value <- output$p.value
    cat("Jarque-Bera p-value for model", i, "is", p_value, "\n")

    if (p_value < 0.05) {
        message("(Reject NULL hypothesis at 5% significance level)")
    } else {
        message("(Do not reject NULL hypothesis at 5% significance level)")
    }
    message("\n")
}


################################################################################
#                      Summary of results from Assignment 1                    #
################################################################################

# The best model according to the AIC test is model 5, and when comparing this to
# the results obtained from running the LjungBox and Jarque-Bera tests, this seems
# to be the case. When running the LjungBox and Jarque-Bera tests on the models,
# we do not want it to reject the null hypothesis. For the LjungBox test,
# we don’t want to reject the null because if it is rejected,
# that means there is autocorrelation in the residuals. For the Jarque-Bera we
# don’t want it to reject the null because this would mean that the residuals
# isn’t normally distributed. As we can see in our output, Model 5 passes the tests
# best (especially the LjungBox), and hence supports the results from the AIC.
# This is considering a significance level of 5%.
