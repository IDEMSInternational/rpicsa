library(here)
library(lubridate)

daily_niger <- read.csv(here("daily_niger_raw.csv"))
names(daily_niger)[1] <- "station_name"
names(daily_niger)[4] <- "month"
names(daily_niger)[6] <- "day"
daily_niger$rain <- NULL
names(daily_niger)[9] <- "rain"

daily_niger$date <- as.Date(daily_niger$date)
daily_niger$month <- lubridate::month(daily_niger$date, label = TRUE)
# Don't want month as ordered factor as ordered is handled differently
# in model functions
class(daily_niger$month) <- "factor"

usethis::use_data(daily_niger, overwrite = TRUE)