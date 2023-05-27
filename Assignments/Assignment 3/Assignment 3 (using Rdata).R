silence <- suppressPackageStartupMessages

silence(library(forecast))
silence(library(lmtest))
silence(library(sandwich))

setwd(
  "E:\\Uni\\Econometrics 2b\\R\\Assignments\\Assignment 3"
)


# Load created dataset
tryCatch({
  load("forecasts_new.Rdata")
}, error = function(e) {
  message("
            The loading of forecasts.Rdata failed. Check so 
            that you have set your working directory to the 
            directory where the forecasts.Rdata file exists
            \n\n"
            )
  message(paste("Error message:", e$message))
})

origin_1 <- 2002 + 11 / 12
n_origins <- length(window(cpif, start = origin_1, end = 2020 + 10 / 12))

par(mfrow = c(3, 2))

# Plots for horizon = 1
ts.plot(forecast_h1$re_arima_plot, 
        main = "ARIMA forecast (recursive)",
        col = "#008080")
lines(window(cpif, start = origin_1), col = "#FF7F50")
legend("topleft", legend = c("Inflation rate",
                            paste(12, "-step forecast")),
                            lty = 1,
                            col = c("#FF7F50", "#008080"))

ts.plot(forecast_h1$re_ar1_plot,
         main = "AR(1) forecast (recursive)",
        col = "#008080")
lines(window(cpif, start = origin_1), col = "#FF7F50")
legend("topleft", legend = c("Inflation rate",
                            paste(12, "-step forecast")),
                            lty = 1,
                            col = c("#FF7F50", "#008080"))

# Plots for horizon = 12
ts.plot(forecast_h12$re_arima_plot,
        main = "ARIMA forecast (recursive)",
        col = "#008080")
lines(window(cpif, start = origin_1), col = "#FF7F50")
legend("topleft", legend = c("Inflation rate",
                            paste(12, "-step forecast")),
                            lty = 1,
                            col = c("#FF7F50", "#008080"))

ts.plot(forecast_h12$re_ar1_plot, 
        main = "AR(1) forecast (recursive)",
        col = "#008080")
lines(window(cpif, start = origin_1), col = "#FF7F50")
legend("topleft", legend = c("Inflation rate",
                            paste(12, "-step forecast")),
                            lty = 1,
                            col = c("#FF7F50", "#008080"))

# Plots for horizon = 24
ts.plot(forecast_h24$re_arima_plot, 
        main = "ARIMA forecast (recursive)",
        col = "#008080")
lines(window(cpif, start = origin_1), col = "#FF7F50")
legend("topleft", legend = c("Inflation rate",
                            paste(12, "-step forecast")),
                            lty = 1,
                            col = c("#FF7F50", "#008080"))

ts.plot(forecast_h24$re_ar1_plot,
        main = "AR(1) forecast (recursive)",
        col = "#008080")
lines(window(cpif, start = origin_1), col = "#FF7F50")
legend("topleft", legend = c("Inflation rate",
                            paste(12, "-step forecast")),
                            lty = 1,
                            col = c("#FF7F50", "#008080"))




# Bias testing

# Define function for extracting pval for NW
test_bias <- function(h, errors) {
  model <- lm(errors[, h] ~ 1)
  NW_cov <- NeweyWest(model, lag = h - 1)
  pval_pos <- 4
  pval <- round(coeftest(model, vcov. = NW_cov)[[pval_pos]], 4)
  return(pval)
}


newey <- function(hmax, model) {

        outcome <- ts(
        data = matrix(NA, nrow = n_origins, ncol = hmax),
        start = origin_1,
        frequency = frequency(cpif)
        )

        for (i in 1:n_origins) {
                # we do not want to include the origin itself
                start_date <- origin_1 + i / 12
                end_date <- start_date + 1 / 12
                outcome[i, ] <- window(cpif, start = start_date, end = end_date)
        }

        colnames(outcome) <- paste0("h=", 1:hmax)

        re_error <- outcome - model

        newey_pval <- do.call(rbind, lapply(1:hmax, test_bias, re_error))

        current_model <- deparse(substitute(model))
        message(cat("Newey p-value for", current_model, "using hmax", hmax, ":"))
        newey_pval
}

# Can not run test on hmax = 1, thus marking those models
# as comments below

# newey(hmax = 1, model = forecast_h12$re_arima_mean)
newey(hmax = 12, model = forecast_h12$re_arima_mean)
newey(hmax = 24, model = forecast_h24$re_arima_mean)

# newey(hmax = 1, model = forecast_h12$re_ar1_mean)
newey(hmax = 12, model = forecast_h12$re_ar1_mean)
newey(hmax = 24, model = forecast_h24$re_ar1_mean)



# prec_pvals <- data.frame(
#   h = 1:hmax,
#   pval = NA
# )


# test_results <- sapply(1:hmax, function(h) {
#   dm.test(re_error[, h], ro_error[, h], alternative = "two.sided", power = 2)
# })


# for (h in 1:hmax) {
#   test <- dm.test(re_error[, h], ro_error[, h], h = h)

#   prec_pvals$pval[h] <- test$p.value
# }
