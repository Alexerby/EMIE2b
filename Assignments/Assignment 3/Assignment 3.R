silence <- suppressPackageStartupMessages

silence(library(forecast))
silence(library(lmtest))
silence(library(sandwich))



#########################################################
#           COMMENTS REGARDING THE DATASET              #
#########################################################
# The dataset contains 7 variables:
#
#       recursive_forecast_h1
#       recursive_forecast_h12
#       recursive_forecast_h24
#
#       rolling_forecast_h1
#       rolling_forecast_h12
#       rolling_forecast_h24
#
#       cpif     (a compiled time series from SCB)
#
#########################################################
#
# If load("forecasts.Rdata") does not execute properly,
# make sure that setwd() is properly configured to the
# directory of forecasts.Rdata


# Load created dataset
tryCatch({
  load("forecasts.Rdata")
}, error = function(e) {
  message("
            The loading of forecasts.Rdata failed. Check so 
            that you have set your working directory to the 
            directory where the forecasts.Rdata file exists
            \n\n"
            )
  message(paste("Error message:", e$message))
})


#########################################################
#          Question 1: Plotting recursive models        #
#########################################################
#
# All the data handling and creation is made in the
# "Data compilation.R" file which is where we call the
# API to SCBs Statistikdatabas (using pxweb).

#########################################################
#          Question 2: Plotting recursive models        #
#########################################################

# Down below we plot the (2 x 3 = 6) models for h = 1, 12, 24

origin_1 <- 2002 + 11 / 12
n_origins <- length(window(cpif, start = origin_1, end = 2020 + 10 / 12))

par(mfrow = c(3, 2))

forecast_plot <- function(model, hmax, type) {
        # Plots for horizon = 1
        ts.plot(model, 
        main = paste(type, "forecast (recursive)"),
        col = "#008080")
        lines(window(cpif, start = origin_1), col = "#FF7F50")
        legend("topleft", legend = c("Inflation rate",
                                paste(hmax, "-step forecast")),
                                lty = 1,
                                col = c("#FF7F50", "#008080"))
}

# Plot our 6 recursive models
forecast_plot(recursive_forecast_h1$re_arima_plot, 1, type = "ARIMA")
forecast_plot(recursive_forecast_h1$re_ar1_plot, 1, type = "AR(1)")
forecast_plot(recursive_forecast_h12$re_arima_plot, 12, type = "ARIMA")
forecast_plot(recursive_forecast_h12$re_ar1_plot, 12, type = "AR(1)")
forecast_plot(recursive_forecast_h24$re_arima_plot, 24, type = "ARIMA")
forecast_plot(recursive_forecast_h24$re_ar1_plot, 24, type = "AR(1)")

#########################################################
#          Question 3: Newey West bias testing          #
#########################################################

# Define function for extracting pval for NW
test_bias <- function(h, errors) {
  model <- lm(errors[, h] ~ 1)
  NW_cov <- NeweyWest(model, lag = h - 1)
  pval_pos <- 4
  pval <- round(coeftest(model, vcov. = NW_cov)[[pval_pos]], 4)
  return(pval)
}


newey <- function(hmax, model) {

        # Create empty matrix
        outcome <- ts(
        data = matrix(NA, nrow = n_origins, ncol = hmax),
        start = origin_1,
        frequency = frequency(cpif)
        )

        # Loop through the origins 
        for (i in 1:n_origins) {
                # we do not want to include the origin itself
                start_date <- origin_1 + i / 12
                end_date <- start_date + 1 / 12
                outcome[i, ] <- window(cpif, start = start_date, end = end_date)
        }

        colnames(outcome) <- paste0("h=", 1:hmax)

        error <- outcome - model

        newey_pval <- do.call(rbind, lapply(1:hmax, test_bias, error))

        current_model <- deparse(substitute(model))

        return(list(error = error, newey = newey_pval))
}


# Store functions for all our recursive and rolling models. The $newey sublist
# is used in this question (3) and the $error sublist for next question

re_nw_arima_h12 <- newey(hmax = 12, model = recursive_forecast_h12$re_arima_mean)
re_nw_arima_h24 <- newey(hmax = 24, model = recursive_forecast_h24$re_arima_mean)

re_nw_ar_h12 <- newey(hmax = 12, model = recursive_forecast_h12$re_ar1_mean)
re_nw_ar_h24 <- newey(hmax = 24, model = recursive_forecast_h24$re_ar1_mean)

ro_nw_arima_h12 <- newey(hmax = 12, model = rolling_forecast_h12$ro_arima_mean)
ro_nw_arima_h24 <- newey(hmax = 24, model = rolling_forecast_h24$ro_arima_mean)

ro_nw_ar_h12 <- newey(hmax = 12, model = rolling_forecast_h12$ro_ar1_mean)
ro_nw_ar_h24 <- newey(hmax = 24, model = rolling_forecast_h24$ro_ar1_mean)


# Newey West for Arima, h = 12
re_nw_arima_h12$newey

# Newey West for Arima, h = 24
re_nw_arima_h24$newey

# Newey West for AR, h = 12
re_nw_ar_h12$newey

# Newey West for AR, h = 12
re_nw_ar_h12$newey


#########################################################
#          Question 4: Diebold-Mariano test             #
#########################################################

# We currently have 2x3 recursive functions, we have
# 3 different horizons, and two types (AR & ARIMA models).
# However, test cant be performed on model with h = 1,
# making this question have 4 predictions.
#
# In addition to that we have also in the data compilation
# script created rolling models, because they need to be used
# for this last question when predicting p-values for the
# Diebold-Mariano test.
#
# This implies that we will need to run this prediction test
# 6 times. Thus, we are again creating a function for this.
#
# For ease of reading we are going to assign the error to
# easy to read variables for this last question.




re_nw_arima_h12$error
ro_nw_arima_h12$error

dm_test <- function(recursive_model, rolling_model, hmax, type) {

        prec_pvals <- data.frame(
        h = 1:hmax,
        pval = NA
        )

        for (h in 1:hmax) {
        test <- dm.test(recursive_model[, h], rolling_model[, h], h = h)

        prec_pvals$pval[h] <- test$p.value
        }

        a <- cat("\nP-values for model type", toupper(type), "with hmax", hmax, ":\n\n")
        a

        prec_pvals
}

# ARIMA models
dm_test(re_nw_arima_h12$error, ro_nw_arima_h12$error, hmax = 12, type = "Arima")
dm_test(re_nw_arima_h24$error, ro_nw_arima_h24$error, hmax = 24, type = "Arima")

# AR models
dm_test(re_nw_ar_h12$error, ro_nw_ar_h12$error, hmax = 12, type = "AR")
dm_test(re_nw_ar_h24$error, ro_nw_ar_h24$error, hmax = 24, type = "AR")
