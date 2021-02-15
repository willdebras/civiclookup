# Packages
library(data.table)
library(rvest)
library(xml2)

# URL / Page load
url <- "https://ballotpedia.org/Elections_calendar"
page <- rvest::html_table(xml2::read_html(url)
                          , fill = TRUE)

# Data Changes
elec_dates <- data.table::data.table(page[[4]])[, date_char := gsub(",", "", Date)]
elec_dates <- elec_dates[i = 2:nrow(elec_dates)
                         , 
                         j = .(STATE = State
                               , DESCRIPTION = Description
                               , TYPE = Type
                               , DATE = as.Date(date_char
                                                , format = "%B %d %Y")
                         )
                         ]

# Save 
data.table::fwrite(elec_dates, "Data/elec_dates.csv")
time <- as.POSIXct(Sys.time(), "Etc/GMT+5")
save(time, file = "Data/time.RData")

