library(dynlm)
library(lmtest)
library(sandwich)
library(Ecdat)
library(mFilter)
library(TS)
library(pxweb)

data("Macrodat", package="Ecdat")

gdp_diff <- diff(Macrodat[, "gdpjp"])
ts.plot(gdp_diff)

class(gdp_diff)


#############################################
#                   AR(1)                   #
#############################################

# First way
formula1 <- gdp_diff ~ L(gdp_diff)

result1 <- dynlm(formula1)
result1

# Second way
formula2 <- gdp_diff ~ lag(gdp_diff, k = -1)
result2 <- dynlm(formula2)
result2

#############################################
#       Test for autocorrelation            #
#############################################

plot(result1$residuals)
plot(residuals(result1))


# How many lags to use?
nlags <- floor(0.75 * nobs(result1)^(1/3))

m <- length(coef(result1))

LjungBox(result1$residuals, lag = 1:nlags, order = m) # Reject H0

########################
# a) 

data(Irates)
head(Irates)

d.r1 <- diff(Irates[, 1])

formula_dynlm <- d.r1 ~ lag(d.r1, k = -1)
formula_lm <- d.r1 ~ lag(d.r1, k = -1)

# dynlm
model1 <- dynlm(formula_dynlm)

# lm
model2 <- lm(formula_lm)

ts.plot(model1$residuals)
ts.plot(model2$residuals)

####################

data_lm <- embed(d.r1, dimension = 2)

model_3 <- lm(data_lm[, 1] ~ data_lm[, 2])

LjungBox(residuals(model1), lags=1:4, order = 2)

##############

model1 <- dynlm(formula_dynlm)

coeftest(model1)
vcov(model1)

HAC <- NeweyWest(model1)
HAC

coeftest(model1, vcov = vcov(model1))
coeftest(model1, vcov = HAC)


data <- pxweb_interactive()
