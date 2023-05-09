# Load required libraries
library(Ecdat)
library(dynlm)
library(TS)
library(tseries)
library(scales)

# Load dataset IncomeUK (from package Ecdat)
data(IncomeUK, package = "Ecdat")


# Function definitions

# Function for comparing p-values to a significance level
# will be used for the Ljung-Box and Jarque-Bera tests
p_value_comparison <- function(test_stat) {

    for (i in 1:5) {
        cat("\nP-values comparing for model", i, ":\n")
        test_stat <- get(paste0("lb_test", i))

        for (j in 1:ncol(test_stat)) {
            if (test_stat[j, "p-value"] > 0.05) {
                cat("Lag", test_stat[j, "lags"], "p-value > \u03B1 (0.05) \u27f6 do not reject NULL hypothesis\n")
            } else {
                cat("Lag", test_stat[j, "lags"], "has p-value \u2264 \u03B1 (0.05) \u27f6 reject NULL hypothesis.\n")
            }
        }
    }
}

################################################################################
#                                   QUESTION 1                                 #
################################################################################

# Initialize empty list for the models
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

for (i in 1:length(model_list)) {
    cat("Summary of model", i)
    print(summary(get(paste0("model_", i))))
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

# Determine the minimum value of the models
best_model_aic <- which.min(aic_values)
cat("Best model based on AIC: ", best_model_aic, "\n")

################################################################################
#                                   QUESTION 3                                 #
################################################################################

#########################################################
#                   Ljung-Box test                      #
#########################################################

for (i in 1:length(model_list)) {
    # Create variables for each models residuals
    residuals_model <- paste0("residuals_model_", i)
    residuals <- residuals(model_list[[i]])
    assign(residuals_model, residuals)

    # Perform Ljung-Box test on residuals
    cat("\n Ljung-Box output for model", i, ":\n")
    print(TS::LjungBox(get(residuals_model)))
}

# Assign variable names for ljung-box test called lb_test1,...,lb_test5
for (i in 1:5) {
    lb_test <- TS::LjungBox(get(paste0("residuals_model_", i)))
    assign(paste0("lb_test", i), lb_test)
}



# Compare p-value with a significance level of 5 percent for our models
p_value_comparison(lb_test)


# We can see from the output of the above that we can not reject
# H_0 when it comes to income_{t-5}

#########################################################
#                   Jarque-Bera test                    #
#########################################################

# Assign variable jb_test1, jb_test2,..., jb_testi for each model
for (i in 1:5) {
    foo <- (get(paste0("residuals_model_", i)))
    jb_test <- jarque.bera.test(foo)
    assign(paste0("jb_test", i), jb_test)
}

# Print jb_test output for each model
for (i in 1:5) {
    cat("\n JB-test", i, ":")
    jb_test <- paste0("jb_test", i)
    jb_output <- get(jb_test)
    print(jb_output)
}

# Compare p-values from the jb_test
p_value_comparison(jb_test)

# Choose significance level
significance_level <- 0.05


for (i in 1:5) {
    p_value <- paste0(get(paste0("jb_test", i))$p.value)
    cat(paste0("\nP-value for model ", i, ": ", p_value, "\n"))
    if (as.numeric(p_value) < significance_level) {
        message(cat("Reject null hypothesis on", percent(significance_level), "signficance level."))
    } else if (as.numeric(p_value) > significance_level) {
        message(cat("Do not reject null hypothesis on", percent(significance_level), "signficance level."))
    }
}