library(rpicsa)
library(dplyr)
library(databook)

# Testing annual_rain function --------------------
data_book <- DataBook$new()
niger <- daily_niger %>%
  filter(year %in% 1950:1951)
data_book$import_data(list(niger = niger))

x_year <- niger %>% 
  group_by(year, station_name) %>% 
  summarise(sum_rain = mean(tmin))

x_month <- niger %>% 
  group_by(year, month, station_name) %>% 
  summarise(sum_rain = mean(tmin))

y_year <- summary_temperature(data = "niger", year = "year",
                     station = "station_name", tmin = "tmin",
                      to = "annual")
y_year <- data_book$get_data_frame("niger_by_station_name_year")

y_month <- summary_temperature(data = "niger", year = "year", month  = "month",
                             station = "station_name", tmin = "tmin",
                             to = "monthly")
y_month <- data_book$get_data_frame("niger_by_station_name_year")

test_that("Returns correct monthly summaries", {
  expect_length(y_month, 5)
})

test_that("Returns correct annual summaries", {
  expect_length(y_year, 5)
})
