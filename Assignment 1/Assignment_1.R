# Load required libraries
library(devtools) # used for convenient installation of local packages (TS)
library(Ecdat)
library(dynlm)
library(TS)


# Load dataset IncomeUK (from package Ecdat)
data(IncomeUK, package = "Ecdat")

################################################################################
#                                   QUESTION 1                                 #
################################################################################

# Defining models
model1 <- dynlm(income ~ L(income, 1), data = IncomeUK)
model2 <- dynlm(income ~ L(income, 1:2), data = IncomeUK)
model3 <- dynlm(income ~ L(income, 1:3), data = IncomeUK)
model4 <- dynlm(income ~ L(income, 1:4), data = IncomeUK)
model5 <- dynlm(income ~ L(income, 1:5), data = IncomeUK)

# Append the models to a list
models <- list(model1, model2, model3, model4, model5)

# Call the summary function for all the models
for (i in 1:length(models)) { 
  cat("Results for Model ", i, ":\n")
  print(summary(models[[i]]))
}


################################################################################
#                                   QUESTION 2                                 #
################################################################################

# Calculating the AIC for the different models
aic_values <- c(AIC(model1), AIC(model2), AIC(model3), AIC(model4), AIC(model5))

# Determine the minimum value of the models
best_model_aic <- which.min(aic_values)
cat("Best model based on AIC: ", best_model_aic, "\n")

################################################################################
#                                   QUESTION 3                                 #
################################################################################

# Create empty list for our residuals
residuals <- list()

# For loop for storing our residuals in our empty list
for (i in 1:length(models)) {
  residuals[[i]] <- residuals(models[[i]])
}

TS::LjungBox()
