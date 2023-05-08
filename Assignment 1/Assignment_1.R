# Load required libraries
library(Ecdat)
library(dynlm)
library(TS)


# Load dataset IncomeUK (from package Ecdat)
data(IncomeUK, package = "Ecdat")

################################################################################
#                                   QUESTION 1                                 #
################################################################################

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

aic_values

# Determine the minimum value of the models
best_model_aic <- which.min(aic_values)
cat("Best model based on AIC: ", best_model_aic, "\n")

################################################################################
#                                   QUESTION 3                                 #
################################################################################


residuals_list <- list()

for (i in 1:length(model_list)) {
    residuals_list[[i]] <- residuals(model_list[[i]])
    assign(paste0("residuals_model_", i), residuals_list[[i]])
}


TS::LjungBox(residuals_model_1)
