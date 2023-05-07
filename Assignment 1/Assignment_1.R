# Load required libraries
library(Ecdat)
library(dynlm)

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


# Producing outputs by calling the summary() function
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)


################################################################################
#                                   QUESTION 2                                 #
################################################################################

# Calculating the AIC for the different models
aic.values <- c(AIC(model1), AIC(model2), AIC(model3), AIC(model4), AIC(model5))

# Determine the minimum value of the models
best.model.aic <- which.min(aic.values)
cat("Best model based on AIC: ", best.model.aic, "\n")


################################################################################
#                                   QUESTION 3                                 #
################################################################################
