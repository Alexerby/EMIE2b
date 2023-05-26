# Initialize lists to store the results of AR(1) model
re_ar1_models <- list()
re_ar1_forecasts <- list()

model_creation_and_printing <- function(hmax) {

  # Data frames for both ARIMA and AR(1)
  re_arima_mean <- ts(data = matrix(NA, nrow = 216, ncol = hmax),
             start = origin_1,
             frequency = frequency(cpif_ts)
  )

  re_ar1_mean <- ts(data = matrix(NA, nrow = 216, ncol = hmax),
             start = origin_1,
             frequency = frequency(cpif_ts)
  )

  colnames(re_arima_mean) <- paste0("h=", 1:hmax)
  colnames(re_ar1_mean) <- paste0("h=", 1:hmax)

  for (i in 1:n_origins) {
    origin <- origin_1 + (i - 1) / 12

    dat <- window(cpif_ts, end = origin)

    # Fit ARIMA model
    m_arima <- auto.arima(dat)
    f_arima <- forecast(m_arima, h = hmax)

    re_arima_mean[i, ] <- f_arima$mean 

    re_arima_models[[i]] <- m_arima
    re_arima_forecasts[[i]] <- f_arima

    # Fit AR(1) model
    m_ar1 <- Arima(dat, order = c(1, 0, 0))
    f_ar1 <- forecast(m_ar1, h = hmax)

    re_ar1_mean[i, ] <- f_ar1$mean 

    re_ar1_models[[i]] <- m_ar1
    re_ar1_forecasts[[i]] <- f_ar1
  }

  # Combine ARIMA forecasts
  re_arima_list <- lapply(re_arima_forecasts, function(x) x[["mean"]])
  re_arima_plot <- do.call(ts.union, re_arima_list)

  # Combine AR(1) forecasts
  re_ar1_list <- lapply(re_ar1_forecasts, function(x) x[["mean"]])
  re_ar1_plot <- do.call(ts.union, re_ar1_list)

  # Plot results
  ts.plot(re_arima_plot, main = paste("Forecasts (recursive) for h=", hmax), col = "red")
  lines(re_ar1_plot, col = "green")
  lines(window(cpif_ts, start = origin_1), col = "blue")
  legend("topleft", legend = c("Inflation rate", "AR(1) forecast", "ARIMA forecast"), lty = 1, col = c("blue", "green", "red"))
}