
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




origin_1 <- 2002 + 11 / 12

par(mfrow = c(3, 2))

# Plots for horizon = 1
ts.plot(forecast_h1$re_arima_plot, main = "ARIMA forecast (recursive)", col = "#008080")
lines(window(cpif, start = origin_1), col = "#FF7F50")
legend("topleft", legend = c("Inflation rate", 
                            paste(12, "-step forecast")),
                            lty = 1,
                            col = c("#FF7F50", "#008080"))

ts.plot(forecast_h1$re_ar1_plot, main = "AR(1) forecast (recursive)", col = "#008080")
lines(window(cpif, start = origin_1), col = "#FF7F50")
legend("topleft", legend = c("Inflation rate", 
                            paste(12, "-step forecast")),
                            lty = 1,
                            col = c("#FF7F50", "#008080"))


# Plots for horizon = 12
ts.plot(forecast_h12$re_arima_plot, main = "ARIMA forecast (recursive)", col = "#008080")
lines(window(cpif, start = origin_1), col = "#FF7F50")
legend("topleft", legend = c("Inflation rate", 
                            paste(12, "-step forecast")),
                            lty = 1,
                            col = c("#FF7F50", "#008080"))

ts.plot(forecast_h12$re_ar1_plot, main = "AR(1) forecast (recursive)", col = "#008080")
lines(window(cpif, start = origin_1), col = "#FF7F50")
legend("topleft", legend = c("Inflation rate", 
                            paste(12, "-step forecast")),
                            lty = 1,
                            col = c("#FF7F50", "#008080"))

# Plots for horizon = 24
ts.plot(forecast_h24$re_arima_plot, main = "ARIMA forecast (recursive)", col = "#008080")
lines(window(cpif, start = origin_1), col = "#FF7F50")
legend("topleft", legend = c("Inflation rate", 
                            paste(12, "-step forecast")),
                            lty = 1,
                            col = c("#FF7F50", "#008080"))

ts.plot(forecast_h24$re_ar1_plot, main = "AR(1) forecast (recursive)", col = "#008080")
lines(window(cpif, start = origin_1), col = "#FF7F50")
legend("topleft", legend = c("Inflation rate", 
                            paste(12, "-step forecast")),
                            lty = 1,
                            col = c("#FF7F50", "#008080"))