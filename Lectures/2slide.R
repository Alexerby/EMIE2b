library(dynlm)
library(lmtest)

set.seed(1)
y <- ts(rnorm(250, sd = 10))
y <- y + .2 * time(y) # y_t= 0.2t+e_t

set.seed(2)
x <- ts(rnorm(250, sd = 10))
x <- x - .2 * time(x) # x_t=−0.2t+e_t

ts.plot(y)
ts.plot(x)

yx <- dynlm(y ~ x)

acf(residuals(yx), plot = FALSE, lag.max = 8)
1.96 / sqrt(250)

# Coefficent test of y_t = a + b_1 x_t + b_2 t + e_t
yxt <- dynlm(y ~ x + trend(x))
coef(yxt)
