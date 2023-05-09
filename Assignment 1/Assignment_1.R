# Load required libraries
library(Ecdat)
library(dynlm)
library(TS)
library(tseries)

# Load dataset IncomeUK (from package Ecdat)
data(IncomeUK, package = "Ecdat")


# Function definitions

# Function for comparing p-values to a significance level
# will be used for the Ljung-Box and Jarque-Bera tests

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

for (i in 1:length(model_list)) {
    cat("Summary of model", i)
    print(summary(get(paste0("model_", i))))
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
    print(TS::LjungBox(get(residuals_model), lags = c(1:i)))
}

# lags = nlags not specified yet 
# nlags <- floor(0.75 * nobs(residuals)^(1/3))