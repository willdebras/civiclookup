# Packages
library(data.table)
library(rvest)
library(xml2)
library(english)

# URL / Page load
url <- "https://ballotpedia.org/Elections_calendar"
page <- rvest::html_table(xml2::read_html(url)
                          , fill = TRUE)

# Data Changes

states <- data.table::data.table(STATE = state.name
                                 , STATE_ABB = state.abb)

elec_dates1 <- data.table::data.table(page[[4]])[, date_char := gsub(",", "", Date)]
elec_dates1 <- merge(elec_dates1, states, by.x = "State", by.y = "STATE")
elec_dates1 <- elec_dates1[i = 2:nrow(elec_dates)
                           , 
                           j = .(STATE = STATE_ABB
                                 , DESCRIPTION = Description
                                 , TYPE = Type
                                 , DATE = as.Date(date_char
                                                  , format = "%B %d %Y")
                           )
]
elec_dates2 <- elec_dates1[,
                           j = .(COUNT = paste0(english::as.english(.N), 
                                                " upcoming "
                                                , ifelse(tolower(TYPE) == "ballot access", "filing", tolower(TYPE))
                                                , ifelse(tolower(TYPE) == "ballot access", ifelse(.N == 1, " deadline.", " deadlines."), ifelse(.N == 1, " date.", " dates."))
                           ))
                           ,
                           by = .(STATE, DATE, TYPE)
]
elec_dates2 <- elec_dates2[, COUNT := paste0(toupper(substring(COUNT, 1, 1))
                                             , substring(COUNT, 2))
]

# Save 
data.table::fwrite(elec_dates1, "Data/elec_dates_full.csv")
data.table::fwrite(elec_dates2, "Data/elec_dates_group.csv")
time <- as.POSIXct(Sys.time(), "Etc/GMT+5")
save(time, file = "Data/time.RData")