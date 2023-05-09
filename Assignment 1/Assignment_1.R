# Load required libraries
library(Ecdat)
library(dynlm)
library(TS)
library(tseries) # used for jarque.bera.test

# Load dataset IncomeUK (from package Ecdat)
data(IncomeUK, package = "Ecdat")

################################################################################
#                          QUESTION 1: Autoregresive models                    #
################################################################################

# Initialize empty list for the models
model_list <- list()

# Creating models and assigning them to variables
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
    model <- get(paste0("model_", i))
    print(summary(model))
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
    output <- TS::LjungBox(residuals, lags = c(1:i)) # save output of test
    cat("\nModel", i, ":\n")

    # An inner for loop for printing P-values for each model
    for (rows_index in 1:nrow(output)) {
        p_value <- output[rows_index, "p-value"]
        cat("P-value of lag", rows_index, "is", round(p_value, 3))

        # Checking if we want to reject H0 or not and
        # printing the output
        if (p_value < 0.05) {
            message("    (reject H0 at 5% significance level)")
        } else {
            message("")
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
    cat("Jarque-Bera p-value for model", i, "is ", p_value, "\n")

    if (p_value < 0.05) {
        message("Reject NULL hypothesis at 5% significance level")
    } else {
        message("Do not reject NULL hypothesis at 5% significance level")
    }
    message("\n")
}
