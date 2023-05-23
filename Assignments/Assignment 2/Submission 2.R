library(lmtest)
library(TS)
library(urca)

# Load Data
data(ChickEgg, package = "lmtest")

# Assign variables
chicken <- ChickEgg[, "chicken"]
egg <- ChickEgg[, "egg"]



################################################################################
#                   QUESTION 1: Order of integration for ChickEgg              #
################################################################################


# We perform Augmented Dickey-Fuller tests in order to decide on the order of
# our functions:
#
#                   Δ^d * chicken_t = τ + ϕ_1(chicken_t−1 − τ ) + ϵ_t
#                   Δ^d * egg = τ + ϕ_1(egg_t−1 − τ ) + ϵ_t
#
# these designate functions of course differ depending on whether the
# definition refer to the none, trend or drift functions.The above
# however refer to the none (no constant, no trend variable).
#

ADF.test(chicken)
ADF.test(egg)

ur.df(chicken, lags = 1)


# Since the functions are already defined for us, this makes it very convenient
# and only requires us to call the function with our designated variables.
#
#   --------------------------------------------------------------------------
#
#
#           INTERPRETATION OF OUTPUT ROW BY ROW (GENERAL):
#
#           ----------------------------------------------------------------
#           ANALYSIS PART
#
#           AR1:            The coefficient for (trend, drift, none) type of
#                           models are printed.
#
#           LAGS:           The lag order (decided by Akaikes Information
#                           Criteria) is selected.
#
#           L-B P-VALUE:    Ljung-Box p-values for all type of models
#
#           J-B P-VALUE:    Jarque-Bera p-values for all type of models
#
#           ----------------------------------------------------------------
#
#           TEST STATISTICS PART
#
#           TAU 3:          Test-statistic with drift and trend
#
#           PHI 3:          F-Statistic of DF test with drift and trend
#
#           TAU 2:          Test-statistic test with drift
#
#           PHI 1:          Joint DF test statistic with drift
#
#           TAU 1:          T-statistic of coefficient (ϕ_1)
#
#           ----------------------------------------------------------------
#
#
#   --------------------------------------------------------------------------
#
#
#           INTERPRETATION OF OUTPUT (CHICKEN):
#           In this function we do not select the method of which the lags
#           are selected, since this function uses the ur.df() function with
#           the parameter of selectlag = "AIC". If we want to customize this
#           ourselves, we instead use the underlying function itself.
#
#
#
#
#
################################################################################
#                   QUESTION 2:               #
################################################################################
