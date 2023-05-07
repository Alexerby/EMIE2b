#################################################################
#                           QUESTION 1                          #
#################################################################

# Load required libraries
library(Ecdat)
library(dynlm)

# Load dataset IncomeUK (from package Ecdat)
data(IncomeUK)

# Specify model using the dynlm() function from the package dynlm.
# We here define our model where income is a model of the first order lag
# with p = 1, 2, 3, 4, 5. In the last argument we specify what dataset to use
model <- dynlm(income ~ L(d(income), 1:5), data = IncomeUK)

# Produce results of our defined model by calling summary() and providing our model as an argument
summary(model)

#################################################################
#                           QUESTION 2                          #
#################################################################
