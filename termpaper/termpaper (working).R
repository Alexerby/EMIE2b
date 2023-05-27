library(pxweb)
library(httr)
library(TS)
library(Ecdat)
library(vars)

###############################################################################
#                           DATA GATHERING & PREPARATION                      #
###############################################################################

domain <- "https://api.scb.se/"

# Delete two rows below 
substring <- c("M03", "M06", "M09", "M12")
substring_pattern <- paste(substring, collapse = "|")


##################################################
#              CPI 2012M01-2022M12              #
##################################################

# CPI query to SCB
cpi_query <-
  list("ContentsCode" = c("000004VV"),
       "Tid" = c(
        "2012M01", "2012M02", "2012M03",
        "2012M04", "2012M05", "2012M06",
        "2012M07", "2012M08", "2012M09",
        "2012M10", "2012M11", "2012M12",
        "2013M01", "2013M02", "2013M03",
        "2013M04", "2013M05", "2013M06",
        "2013M07", "2013M08", "2013M09",
        "2013M10", "2013M11", "2013M12",
        "2014M01", "2014M02", "2014M03",
        "2014M04", "2014M05", "2014M06",
        "2014M07", "2014M08", "2014M09",
        "2014M10", "2014M11", "2014M12",
        "2015M01", "2015M02", "2015M03",
        "2015M04", "2015M05", "2015M06",
        "2015M07", "2015M08", "2015M09",
        "2015M10", "2015M11", "2015M12",
        "2016M01", "2016M02", "2016M03",
        "2016M04", "2016M05", "2016M06",
        "2016M07", "2016M08", "2016M09",
        "2016M10", "2016M11", "2016M12",
        "2017M01", "2017M02", "2017M03",
        "2017M04", "2017M05", "2017M06",
        "2017M07", "2017M08", "2017M09",
        "2017M10", "2017M11", "2017M12",
        "2018M01", "2018M02", "2018M03",
        "2018M04", "2018M05", "2018M06",
        "2018M07", "2018M08", "2018M09",
        "2018M10", "2018M11", "2018M12",
        "2019M01", "2019M02", "2019M03",
        "2019M04", "2019M05", "2019M06",
        "2019M07", "2019M08", "2019M09",
        "2019M10", "2019M11", "2019M12",
        "2020M01", "2020M02", "2020M03",
        "2020M04", "2020M05", "2020M06",
        "2020M07", "2020M08", "2020M09",
        "2020M10", "2020M11", "2020M12",
        "2021M01", "2021M02", "2021M03",
        "2021M04", "2021M05", "2021M06",
        "2021M07", "2021M08", "2021M09",
        "2021M10", "2021M11", "2021M12",
        "2022M01", "2022M02", "2022M03",
        "2022M04", "2022M05", "2022M06",
        "2022M07", "2022M08", "2022M09",
        "2022M10", "2022M11", "2022M12"
        ))

# Download data
cpi_data <-
  pxweb_get(url = paste0(domain, 
                        "/OV0104/v1/doris/en/ssd/PR/PR0101/PR0101A/KPItotM"),
                        query = cpi_query)

# Convert to data.frame
cpi_df <- as.data.frame(cpi_data,
                        column.name.type = "text",
                                            variable.value.type = "text")

##################################################
#               M1 2012M01-2022M12               #
##################################################

# M1 query to SCB
monthly_m1_query <-
  list("Penningm" = c("5LLM1.1E.NEP.V.A"),
       "ContentsCode" = c("0000000G"),
       "Tid" = c(
        "2012M01", "2012M02", "2012M03",
        "2012M04", "2012M05", "2012M06",
        "2012M07", "2012M08", "2012M09",
        "2012M10", "2012M11", "2012M12",
        "2013M01", "2013M02", "2013M03",
        "2013M04", "2013M05", "2013M06",
        "2013M07", "2013M08", "2013M09",
        "2013M10", "2013M11", "2013M12",
        "2014M01", "2014M02", "2014M03",
        "2014M04", "2014M05", "2014M06",
        "2014M07", "2014M08", "2014M09",
        "2014M10", "2014M11", "2014M12",
        "2015M01", "2015M02", "2015M03",
        "2015M04", "2015M05", "2015M06",
        "2015M07", "2015M08", "2015M09",
        "2015M10", "2015M11", "2015M12",
        "2016M01", "2016M02", "2016M03",
        "2016M04", "2016M05", "2016M06",
        "2016M07", "2016M08", "2016M09",
        "2016M10", "2016M11", "2016M12",
        "2017M01", "2017M02", "2017M03",
        "2017M04", "2017M05", "2017M06",
        "2017M07", "2017M08", "2017M09",
        "2017M10", "2017M11", "2017M12",
        "2018M01", "2018M02", "2018M03",
        "2018M04", "2018M05", "2018M06",
        "2018M07", "2018M08", "2018M09",
        "2018M10", "2018M11", "2018M12",
        "2019M01", "2019M02", "2019M03",
        "2019M04", "2019M05", "2019M06",
        "2019M07", "2019M08", "2019M09",
        "2019M10", "2019M11", "2019M12",
        "2020M01", "2020M02", "2020M03",
        "2020M04", "2020M05", "2020M06",
        "2020M07", "2020M08", "2020M09",
        "2020M10", "2020M11", "2020M12",
        "2021M01", "2021M02", "2021M03",
        "2021M04", "2021M05", "2021M06",
        "2021M07", "2021M08", "2021M09",
        "2021M10", "2021M11", "2021M12",
        "2022M01", "2022M02", "2022M03",
        "2022M04", "2022M05", "2022M06",
        "2022M07", "2022M08", "2022M09",
        "2022M10", "2022M11", "2022M12"
        ))

# Download data
m1_data <-
  pxweb_get(url = paste0(domain, 
                        "OV0104/v1/doris/en/ssd/START/FM/FM5001/FM5001A/FM5001SDDSPM"), 
                        query = monthly_m1_query)

# Convert to data.frame
m1_df <- as.data.frame(m1_data,
                       column.name.type = "text",
                       variable.value.type = "text")

# Delete the redundant column stating "M1" each row
m1_df <- m1_df[-1]


#####################################################
#                 Clean datasets                    #
#               and create tseries                  #
#####################################################

# Rename "Money supply, SEK millions" --> "m1_supply" in m1_df
colnames(m1_df)[colnames(m1_df) == "Money supply, SEK millions"] = "m1_supply"

# Rename "Annual changes" --> "inflation" in cpi_df
colnames(cpi_df)[colnames(cpi_df) == "Annual changes"] = "inflation"


inflation <- ts(cpi_df$inflation, start = 2012, frequency = 12,)
m1 <- ts(m1_df$m1_supply, start = 2012, frequency = 12)

combined_ts <- ts.intersect(inflation, m1)

# Set up the first plot
par(mar = c(5, 4, 4, 6))
plot(combined_ts[, "inflation"],
     type = "l",
     col = 2,
     xlab = "Year",
     ylab = "Inflation YoY-%",
     ylim = c(-1, 12.5))

# Set up the second plot
par(new = TRUE)
plot(combined_ts[, "m1"],
     type = "l",
     col = 4,
     xlab = "",
     ylab = "",
     ylim = c(min(combined_ts[, "m1"]), max(combined_ts[, "m1"])),
     axes = FALSE)

# Add the second y-axis
axis(side = 4, at = pretty(range(combined_ts[, "m1"])))
mtext("Money supply, M1 (MSEK)", side = 4, line = 3)



#####################################################
#                Test stationarity                  #
#####################################################

########################################
#  Testing inflation for stationarity  #
########################################

ADF.test(cpi_df$inflation)      # Can not reject H_0: unit root @ 5% sig. level
foc_inflation <- diff(cpi_df$inflation)
ADF.test(foc_inflation)         # Can reject H_0: unit root @ 5% sig. level

#########################################
# Testing money supply for stationarity #
#########################################

ADF.test(m1_df$m1_supply)       # Can not reject H_0: unit root @ 5% sig. level
foc_m1 <- diff(m1_df$m1_supply)
ADF.test(foc_m1)                # Can reject H_0: unit root @ 5% sig. level


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

# Create ts objects of variables
foc_inflation <- ts(foc_inflation, start = 2012, frequency = 4)
foc_m1 <- ts(foc_m1, start = 2012, frequency = 4)

differenced_combined_ts <- ts.intersect(foc_inflation, foc_m1)


var_select <- VARselect(differenced_combined_ts, type = "both")

#####################################################
#                  Create VAR model                 #
#####################################################

var_result <- VAR(differenced_combined_ts, p = 7, type = "both")


?causality

causality(var_result, cause = "foc_m1")$Granger
causality(var_result, cause = "foc_inflation")$Granger


# USE THIS
irf_m1_to_inflation <- irf(var_result, impulse = "foc_m1", response = "foc_inflation", n.ahead = 40)
plot(irf_m1_to_inflation)

# USE THIS
irf_inflation_to_m1 <- irf(var_result, impulse = "foc_inflation", response = "foc_m1", n.ahead = 40)
plot(irf_inflation_to_m1)
