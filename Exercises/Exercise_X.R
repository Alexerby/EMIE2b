###############################################
#                   Exercise 3                #
###############################################

library(Ecdat)
library(forecast)
library(TS)

data(MoneyUS)


# Define Model
model <- ts(MoneyUS[, "m"], frequency = 4)
model_auto_arima <- auto.arima(model)
model_auto_arima

# auto.arima on the model
ADF.test(model_auto_arima)
