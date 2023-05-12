# Load required libraries
library(Ecdat)
library(dynlm)
library(TS)
library(tseries) # used for jarque.bera.test

# Load dataset IncomeUK (from package Ecdat)
data(IncomeUK, package = "Ecdat")
income <- IncomeUK[, "income"]  # Rename the variable to avoid conflict

################################################################################
#                          QUESTION 1: Autoregressive models                    #
################################################################################

# Create lags
lags <- embed(income, 5)
lags <- lags[1:(length(income) - 5), ]  # Keep only the relevant rows

for (i in 1:5) {
  assign(paste0("lag_", i), lags[, i])
}

model1 <- lm(income[6:length(income)] ~ lag_1)
model2 <- lm(income[6:length(income)] ~ lag_1 + lag_2)
model3 <- lm(income[6:length(income)] ~ lag_1 + lag_2 + lag_3)
model4 <- lm(income[6:length(income)] ~ lag_1 + lag_2 + lag_3 + lag_4)
model5 <- lm(income[6:length(income)] ~ lag_1 + lag_2 + lag_3 + lag_4 + lag_5)

# nobs(model1)
# nobs(model2)
# nobs(model3)
# nobs(model4)
# nobs(model5)

# model1
# model2
# model3
# model4
# model5