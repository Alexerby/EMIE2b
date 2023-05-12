# Load required libraries
library(Ecdat)
library(dynlm)
library(TS)
library(tseries) # used for jarque.bera.test

# Load dataset IncomeUK (from package Ecdat)
data(IncomeUK, package = "Ecdat")
data <- IncomeUK[, "income"]


################################################################################
#                          QUESTION 1: Autoregresive models                    #
################################################################################


# Create lags
for (i in 1:5) {
    lag <- lag(data, k = i)
    assign(paste0("lag_", i), lag)
}


model1 <- lm(data ~ lag_1)
model2 <- lm(data ~ lag_1 + lag_2)
model3 <- lm(data ~ lag_1 + lag_2 + lag_3)
model4 <- lm(data ~ lag_1 + lag_2 + lag_3 + lag_4)
model5 <- lm(data ~ lag_1 + lag_2 + lag_3 + lag_4 + lag_5)


nobs(model1)
nobs(model2)
nobs(model3)
nobs(model4)
nobs(model5)


model1
model2
model3
model4
model5

for (i in 1:5) {
    embedded_data <- embed(IncomeUK, dimension = i)

    # Create the formula for the model
    formula <- paste("data ~", paste0("lag_", 1:i, collapse = " + "))

    # Fit the model and store it in the list
    model_list[[i]] <- lm(formula, data = IncomeUK)
}



# Initialize empty list for the models

# Creating models and assigning them to variables
for (lag in 1:5) {
    # Fit the model with the current lag value
    model <- dynlm(d(income) ~ L(d(income), 1:lag), data = na.omit(IncomeUK))

    # Add the model object to the list
    model_list[[lag]] <- model

    # Assign the model object to a variable
    # with a name that follows the pattern "model_i"
    assign(paste0("model_", lag), model_list[[lag]])
}

for (i in 1:length(model_list)) {
    cat("Summary of model", i)
    model <- get(paste0("model_", i))
    print(summary(model))
    message(paste(rep("- ", 35), collapse = ""))
}
