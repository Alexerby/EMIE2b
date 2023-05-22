library(pxweb)
library(httr)
library(dplyr)
library(zoo)

###############################################################################
#                           DATA GATHERING & PREPARATION                      #
###############################################################################

domain <- "https://api.scb.se/"

##################################################
#            Quarterly GDP 2012 - 2022           #
##################################################

gdp_api_path <- "OV0104/v1/doris/en/ssd/NR/NR0103/NR0103B/NR0103ENS2010T10SKv"

# Quarterly GDP API link
quarterly_gdp_api <- paste0(domain, gdp_api_path)

# PXWEB query
quarterly_gdp_query <-
  list("Anvandningstyp" = c("BNPM"),
       "ContentsCode" = c("NR0103CE"),
       "Tid" = c(
        "2012K1", "2012K2", "2012K3", "2012K4",  # 2012
        "2013K1", "2013K2", "2013K3", "2013K4",  #  .
        "2014K1", "2014K2", "2014K3", "2014K4",  #  .
        "2015K1", "2015K2", "2015K3", "2015K4",  #  .
        "2016K1", "2016K2", "2016K3", "2016K4",  #  .
        "2017K1", "2017K2", "2017K3", "2017K4",  #  .
        "2018K1", "2018K2", "2018K3", "2018K4",  #  .
        "2019K1", "2019K2", "2019K3", "2019K4",  #  .
        "2020K1", "2020K2", "2020K3", "2020K4",  #  .
        "2021K1", "2021K2", "2021K3", "2021K4",  #  .
        "2022K1", "2022K2", "2022K3", "2022K4")) # 2022

# Download data
gdp_data <-
  pxweb_get(url = quarterly_gdp_api,
            query = quarterly_gdp_query)


# Convert to data.frame
gdp_df <- as.data.frame(gdp_data, column.name.type = "text", variable.value.type = "text")

##################################################
#                   POLICY RATE                  #
##################################################



##################################################
#       QUARTERLY M1 SUPPLY GDP 2012 - 2022      #
##################################################

m1_api_path <- "OV0104/v1/doris/en/ssd/FM/FM5001/FM5001A/FM5001SDDSPM"

monthly_m1_api <- paste0(domain, m1_api_path)

# PXWEB query 
monthly_m1_query <-
  list("Penningm" = c("5LLM1.1E.NEP.V.A"),
       "ContentsCode" = c("0000000G"),
       "Tid" = c(
        "2012M01", "2012M02", "2012M03", "2012M04",
        "2012M05", "2012M06", "2012M07", "2012M08",
        "2012M09", "2012M10", "2012M11", "2012M12",
        "2013M01", "2013M02", "2013M03", "2013M04",
        "2013M05", "2013M06", "2013M07", "2013M08",
        "2013M09", "2013M10", "2013M11", "2013M12",
        "2014M01", "2014M02", "2014M03", "2014M04",
        "2014M05", "2014M06", "2014M07", "2014M08",
        "2014M09", "2014M10", "2014M11", "2014M12",
        "2015M01", "2015M02", "2015M03", "2015M04",
        "2015M05", "2015M06", "2015M07", "2015M08",
        "2015M09", "2015M10", "2015M11", "2015M12",
        "2016M01", "2016M02", "2016M03", "2016M04",
        "2016M05", "2016M06", "2016M07", "2016M08",
        "2016M09", "2016M10", "2016M11", "2016M12",
        "2017M01", "2017M02", "2017M03", "2017M04",
        "2017M05", "2017M06", "2017M07", "2017M08",
        "2017M09", "2017M10", "2017M11", "2017M12",
        "2018M01", "2018M02", "2018M03", "2018M04",
        "2018M05", "2018M06", "2018M07", "2018M08",
        "2018M09", "2018M10", "2018M11", "2018M12",
        "2019M01", "2019M02", "2019M03", "2019M04",
        "2019M05", "2019M06", "2019M07", "2019M08",
        "2019M09", "2019M10", "2019M11", "2019M12",
        "2020M01", "2020M02", "2020M03", "2020M04",
        "2020M05", "2020M06", "2020M07", "2020M08",
        "2020M09", "2020M10", "2020M11", "2020M12",
        "2021M01", "2021M02", "2021M03", "2021M04",
        "2021M05", "2021M06", "2021M07", "2021M08",
        "2021M09", "2021M10", "2021M11", "2021M12",
        "2022M01", "2022M02", "2022M03", "2022M04",
        "2022M05", "2022M06", "2022M07", "2022M08",
        "2022M09", "2022M10", "2022M11", "2022M12"
        ))

# Download data
m1_data <-
  pxweb_get(url = monthly_m1_api, query = monthly_m1_query)

# Convert to data.frame
m1_df <- as.data.frame(m1_data,
                       column.name.type = "text",
                       variable.value.type = "text")

# Make the data quarterly in order to match the number of rows
substring <- c("M03", "M06", "M09", "M12")
substring_pattern <- paste(substring, collapse = "|")
m1_df <- m1_df[grepl(substring_pattern, m1_df$month), ]

year_quarter <- gdp_df['quarter']
gdp_data <- gdp_df[3]
money_supply <- m1_df[3]

main_df <- cbind(year_quarter, gdp_data, money_supply)
colnames(main_df)[2] <- "GDP_MSEK"
colnames(main_df)[3] <- "M1_MSEK"

# Create column for income velocity of money
main_df$velocity <- main_df$GDP_MSEK / main_df$M1_MSEK


