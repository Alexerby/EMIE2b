library(forecast)
library(dynlm)
library(sandwich)
library(lmtest)
library(pxweb)


# This scripts compiles all the necessary data for assignment 3, however
# we have placed this in a separate file since the compiling of all models
# takes a long time. There will be a supplied .Rdata file in order for the
# assessor of this assignment to not have to go through the process of
# compiling all models.

#############################################################
#     PART 1: QUERYING STATISTIKDATABASEN THROUGH PXWEB API #
#############################################################

pxweb_query_list <-
  list("ContentsCode" = c("PR0101F1"),
       "Tid" = c(
         "1987M01", "1987M02", "1987M03", "1987M04", "1987M05", "1987M06",
         "1987M07", "1987M08", "1987M09", "1987M10", "1987M11", "1987M12",
         "1988M01", "1988M02", "1988M03", "1988M04", "1988M05", "1988M06",
         "1988M07", "1988M08", "1988M09", "1988M10", "1988M11", "1988M12",
         "1989M01", "1989M02", "1989M03", "1989M04", "1989M05", "1989M06",
         "1989M07", "1989M08", "1989M09", "1989M10", "1989M11", "1989M12",
         "1990M01", "1990M02", "1990M03", "1990M04", "1990M05", "1990M06",
         "1990M07", "1990M08", "1990M09", "1990M10", "1990M11", "1990M12",
         "1991M01", "1991M02", "1991M03", "1991M04", "1991M05", "1991M06",
         "1991M07", "1991M08", "1991M09", "1991M10", "1991M11", "1991M12",
         "1992M01", "1992M02", "1992M03", "1992M04", "1992M05", "1992M06",
         "1992M07", "1992M08", "1992M09", "1992M10", "1992M11", "1992M12",
         "1993M01", "1993M02", "1993M03", "1993M04", "1993M05", "1993M06",
         "1993M07", "1993M08", "1993M09", "1993M10", "1993M11", "1993M12",
         "1994M01", "1994M02", "1994M03", "1994M04", "1994M05", "1994M06",
         "1994M07", "1994M08", "1994M09", "1994M10", "1994M11", "1994M12",
         "1995M01", "1995M02", "1995M03", "1995M04", "1995M05", "1995M06",
         "1995M07", "1995M08", "1995M09", "1995M10", "1995M11", "1995M12",
         "1996M01", "1996M02", "1996M03", "1996M04", "1996M05", "1996M06",
         "1996M07", "1996M08", "1996M09", "1996M10", "1996M11", "1996M12",
         "1997M01", "1997M02", "1997M03", "1997M04", "1997M05", "1997M06",
         "1997M07", "1997M08", "1997M09", "1997M10", "1997M11", "1997M12",
         "1998M01", "1998M02", "1998M03", "1998M04", "1998M05", "1998M06",
         "1998M07", "1998M08", "1998M09", "1998M10", "1998M11", "1998M12",
         "1999M01", "1999M02", "1999M03", "1999M04", "1999M05", "1999M06",
         "1999M07", "1999M08", "1999M09", "1999M10", "1999M11", "1999M12",
         "2000M01", "2000M02", "2000M03", "2000M04", "2000M05", "2000M06",
         "2000M07", "2000M08", "2000M09", "2000M10", "2000M11", "2000M12",
         "2001M01", "2001M02", "2001M03", "2001M04", "2001M05", "2001M06",
         "2001M07", "2001M08", "2001M09", "2001M10", "2001M11", "2001M12",
         "2002M01", "2002M02", "2002M03", "2002M04", "2002M05", "2002M06",
         "2002M07", "2002M08", "2002M09", "2002M10", "2002M11", "2002M12",
         "2003M01", "2003M02", "2003M03", "2003M04", "2003M05", "2003M06",
         "2003M07", "2003M08", "2003M09", "2003M10", "2003M11", "2003M12",
         "2004M01", "2004M02", "2004M03", "2004M04", "2004M05", "2004M06",
         "2004M07", "2004M08", "2004M09", "2004M10", "2004M11", "2004M12",
         "2005M01", "2005M02", "2005M03", "2005M04", "2005M05", "2005M06",
         "2005M07", "2005M08", "2005M09", "2005M10", "2005M11", "2005M12",
         "2006M01", "2006M02", "2006M03", "2006M04", "2006M05", "2006M06",
         "2006M07", "2006M08", "2006M09", "2006M10", "2006M11", "2006M12",
         "2007M01", "2007M02", "2007M03", "2007M04", "2007M05", "2007M06",
         "2007M07", "2007M08", "2007M09", "2007M10", "2007M11", "2007M12",
         "2008M01", "2008M02", "2008M03", "2008M04", "2008M05", "2008M06",
         "2008M07", "2008M08", "2008M09", "2008M10", "2008M11", "2008M12",
         "2009M01", "2009M02", "2009M03", "2009M04", "2009M05", "2009M06",
         "2009M07", "2009M08", "2009M09", "2009M10", "2009M11", "2009M12",
         "2010M01", "2010M02", "2010M03", "2010M04", "2010M05", "2010M06",
         "2010M07", "2010M08", "2010M09", "2010M10", "2010M11", "2010M12",
         "2011M01", "2011M02", "2011M03", "2011M04", "2011M05", "2011M06",
         "2011M07", "2011M08", "2011M09", "2011M10", "2011M11", "2011M12",
         "2012M01", "2012M02", "2012M03", "2012M04", "2012M05", "2012M06",
         "2012M07", "2012M08", "2012M09", "2012M10", "2012M11", "2012M12",
         "2013M01", "2013M02", "2013M03", "2013M04", "2013M05", "2013M06",
         "2013M07", "2013M08", "2013M09", "2013M10", "2013M11", "2013M12",
         "2014M01", "2014M02", "2014M03", "2014M04", "2014M05", "2014M06",
         "2014M07", "2014M08", "2014M09", "2014M10", "2014M11", "2014M12",
         "2015M01", "2015M02", "2015M03", "2015M04", "2015M05", "2015M06",
         "2015M07", "2015M08", "2015M09", "2015M10", "2015M11", "2015M12",
         "2016M01", "2016M02", "2016M03", "2016M04", "2016M05", "2016M06",
         "2016M07", "2016M08", "2016M09", "2016M10", "2016M11", "2016M12",
         "2017M01", "2017M02", "2017M03", "2017M04", "2017M05", "2017M06",
         "2017M07", "2017M08", "2017M09", "2017M10", "2017M11", "2017M12",
         "2018M01", "2018M02", "2018M03", "2018M04", "2018M05", "2018M06",
         "2018M07", "2018M08", "2018M09", "2018M10", "2018M11", "2018M12",
         "2019M01", "2019M02", "2019M03", "2019M04", "2019M05", "2019M06",
         "2019M07", "2019M08", "2019M09", "2019M10", "2019M11", "2019M12",
         "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06",
         "2020M07", "2020M08", "2020M09", "2020M10", "2020M11", "2020M12",
         "2021M01", "2021M02", "2021M03", "2021M04", "2021M05", "2021M06",
         "2021M07", "2021M08", "2021M09", "2021M10", "2021M11", "2021M12",
         "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06",
         "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12"))

px_data <- pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/en/ssd/PR/PR0101/PR0101G/KPIF",
            query = pxweb_query_list)

# Convert to data.frame 
cpif_df <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
colnames(cpif_df)[colnames(cpif_df) == "CPIF, annual changes, 1987=100"] = "cpif"

cpif <- ts(cpif_df[, "cpif"], start = 1987, frequency = 12) 


# Create first origin
origin_1 <- 2002 + 11/12

n_origins <- length(window(cpif, start = origin_1, end = 2020 + 10/12))

# Create lists for our recursive models and forecasts
re_arima_models <- list()
re_arima_forecasts <- list()

re_ar1_models <- list()
re_ar1_forecasts <- list()


########################################
#         Models for different h
########################################

models_forecast <- function(hmax) {


  # Create matrix for our ARIMA models
  re_arima_mean <- ts(data = matrix(NA, nrow = 216, ncol = hmax),
          start = origin_1,
          frequency = frequency(cpif)
  )

  # Create matrix for our AR(1) models
  re_ar1_mean <- ts(data = matrix(NA, nrow = 216, ncol = hmax),
            start = origin_1,
            frequency = frequency(cpif)
)

  # Add title for our ARIMA matrix: h = 1, 2,.., hmax
  colnames(re_arima_mean) <- paste0("h=", 1:hmax)

  # Add title for our AR matrix: h = 1, 2,.., hmax
  colnames(re_ar1_mean) <- paste0("h=", 1:hmax)


  for (i in 1:n_origins) {

    origin <- origin_1 + (i - 1) / 12 # calculates origin monthly (1/12)

    dat <- window(cpif, end = origin) # subsetting our ts up to above defined origin

    #############################
    #   FITTING ARIMA MODELS    #
    #############################

    # Constructing ARIMA model from dat object
    model_arima <- auto.arima(dat)

    # Forecast the arima model
    forecast_arima <- forecast(model_arima, h = hmax)

    # Fill matrix with a newly created column "mean"
    re_arima_mean[i, ] <- forecast_arima$mean

    # Append models to our respective lists
    re_arima_models[[i]] <- model_arima
    re_arima_forecasts[[i]] <- forecast_arima


    #############################
    #    FITTING AR(1) MODELS   #
    #############################

    # Constructing AR(1) model from same dat obj
    model_ar1 <- Arima(dat, order = c(1, 0, 0))

    # Forecast ARIMA model
    forecast_ar1 <- forecast(model_ar1, h = hmax)

    # Append the mean value to matrix
    re_ar1_mean[i, ] <- forecast_ar1$mean

    # Append models to our respective lists
    re_ar1_models[[i]] <- model_ar1
    re_ar1_forecasts[[i]] <- forecast_ar1

  }

  # Create anonymous function
  # (creating and using function at same time, 
  # here applying it at our re_forecasts list)
  re_arima_list <- lapply(re_arima_forecasts, function(x) x[["mean"]])
  re_arima_plot <- do.call(ts.union, re_arima_list)

  # Same for AR(1) models
  re_ar1_list <- lapply(re_ar1_forecasts, function(x) x[["mean"]])
  re_ar1_plot <- do.call(ts.union, re_ar1_list)

  return(list(re_arima_plot = re_arima_plot, re_ar1_plot = re_ar1_plot))

}

# Store models in variables
forecast_h1 <- models_forecast(1)
forecast_h12 <- models_forecast(12)
forecast_h24 <- models_forecast(24)

# Save to .Rdata file
save(
  cpif,
  forecast_h1,
  forecast_h12,
  forecast_h24,
  file = "forecasts1.Rdata"
)
