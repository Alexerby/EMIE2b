library(pxweb)
library(httr)
library(TS)
library(Ecdat)
library(vars)
library(tseries)
library(TS)


# PXWEB query 
cpi_query <-
  list("ContentsCode"=c("000004VV"),
       "Tid" = c(
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
            "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12",
            "2023M01", "2023M02", "2023M03", "2023M04"))

# Download data
px_data <-
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/en/ssd/PR/PR0101/PR0101A/KPItotM",
            query = cpi_query)

# Convert to data.frame
cpi_df <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")


# PXWEB query
m1_query <-
  list("Penningm" = c("5LLM1.1E.NEP.V.A"),
       "ContentsCode" = c("0000000G"),
       "Tid" = c(
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
        "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12",
        "2023M01", "2023M02", "2023M03", "2023M04"))

# Download data
px_data <-
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/en/ssd/FM/FM5001/FM5001A/FM5001SDDSPM",
            query = m1_query)

# Convert to dataframe
m1_df <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
m1_df <- m1_df[-1]

#####################################################
#                 Clean datasets                    #
#               and create tseries                  #
#####################################################

# Rename "Money supply, SEK millions" --> "m1_supply" in m1_df
colnames(m1_df)[colnames(m1_df) == "Money supply, SEK millions"] <- "m1_supply"

# Rename "Annual changes" --> "inflation" in cpi_df
colnames(cpi_df)[colnames(cpi_df) == "Annual changes"] <- "inflation"


inflation <- ts(cpi_df$inflation, start = 2004, frequency = 12)
m1 <- ts(m1_df$m1_supply, start = 2004, frequency = 12)

combined_ts <- ts.intersect(inflation, m1)



# Set up the first plot
par(mar = c(5, 4, 4, 6))
plot(combined_ts[, "inflation"],
     type = "l",
     col = "#133455",
     xlab = "Year",
     ylab = "Inflation YoY-%",
     ylim = c(-1, 12.5)
     )

# Set up the second plot
par(new = TRUE)
plot(combined_ts[, "m1"],
     type = "l",
     col = "#FF6600",
     xlab = "",
     ylab = "",
     ylim = c(min(combined_ts[, "m1"]), max(combined_ts[, "m1"])),
     axes = FALSE)

# Add the second y-axis
axis(side = 4, at = pretty(range(combined_ts[, "m1"])))
mtext("Money supply, M1 (MSEK)", side = 4, line = 3)
legend(
     "topleft", legend = c("Inflation", "M1"),
     col = c("#133455", "#FF6600"),
     lty = 1, bty = "n"
)




#######################################################################
#                                                                     #
#                        Diagnostic Testing                           #
#                                                                     #
#######################################################################

#####################################################
#                Test autocorrelation               #
#####################################################
acf(m1, lag.max = 80)
acf(inflation, lag.max = 20)

#####################################################
#   Examine residuals distribution                  #
#####################################################

jarque.bera.test(residuals(cpi_df$inflation))
jarque.bera.test(residuals(m1))
residuals(m1_df)
#####################################################
#                Test stationarity                  #
#####################################################

########################################
#  Testing inflation for stationarity  #
########################################
ADF.test(cpi_df$inflation)
      # Can not reject H_0: unit root @ 5% sig. level
fod_inflation <- diff(cpi_df$inflation)
ADF.test(fod_inflation)         # Can reject H_0: unit root @ 5% sig. level

#########################################
# Testing money supply for stationarity #
#########################################
ADF.test(m1_df$m1_supply)       # Can not reject H_0: unit root @ 5% sig. level
fod_m1 <- diff(m1_df$m1_supply)
ADF.test(fod_m1)                # Can reject H_0: unit root @ 5% sig. level


# Note both inflation and money supply are of first order integration I(1)
# so we need to perform an Engle-Granger-ADF test for cointegration

#####################################################
#          Test for cointegration (both I(1))       #
#####################################################

engle_granger <- lm(cpi_df$inflation ~ m1_df$m1_supply)
ADF.test(residuals(engle_granger))  # We reject H_=: no cointegration @ 5% sig. level
# |tau3| < |(cval @ 5% sig. level) implies ⇒ No cointegration


#####################################################
#           Identify VAR lag order                  #
#####################################################

# Evaluate if we want to use none, drift or trend when
# constructing our VAR model
plot(decompose(m1))
plot(decompose(inflation))

# Create ts objects of variables
fod_inflation <- ts(fod_inflation, start = 2004, frequency = 12)
fod_m1 <- ts(fod_m1, start = 2004, frequency = 12)

differenced_combined_ts <- ts.intersect(fod_inflation, fod_m1)


VARselect(differenced_combined_ts, type = "both")

#####################################################
#                  Create VAR model                 #
#####################################################

var_result <- VAR(differenced_combined_ts, p = 6, type = "both")


# Finish the diagnostic test of multivariate autocorrelation
# which could not be finished during our diagnostics section.


# Multivariate Ljung-Box test
serial.test(var_result)

# Jarque-Bera test
normality.test(var_result)$jb.mul$JB



causality(var_result, cause = "fod_m1")$Granger
causality(var_result, cause = "fod_inflation")$Granger


# USE THIS
irf_m1_to_inflation <- irf(
                         var_result,
                         impulse = "fod_m1",
                         response = "fod_inflation",
                         n.ahead = 15)

plot(
irf_m1_to_inflation,
main = "Impulse Response from shock in M1",
ylab = "Inflation (Annual YoY-%)"
)


# USE THIS
irf_inflation_to_m1 <- irf(
                         var_result, 
                         impulse = "fod_inflation",
                         response = "fod_m1",
                         n.ahead = 15)
plot(
irf_inflation_to_m1,
main = "Impulse Response from shock in Inflation",
ylab = "Money supply, (M1)"
)
