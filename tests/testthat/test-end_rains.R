library(rpicsa)
library(dplyr)
library(databook)

# Testing annual_rain function --------------------
library(databook)
data_book <- DataBook$new()
daily_data <- rpicsa::daily_niger %>%
  filter(year <= 1950) %>%
  filter(year > 1945) %>%
  mutate(year = as.numeric(year)) %>%
  filter(station_name == "Agades")
data_book$import_data(list(daily_data = daily_data))

devtools::load_all()
daily_data <- data_book$get_data_frame("daily_data")

end_rains(data = "daily_data", date_time = "date", station = "station_name",
          year = "year", rain = "rain",
          start_day = 121, end_day = 300)
daily_data_by_station_name_year <- data_book$get_data_frame("daily_data_by_station_name_year")

end_rains(data = "daily_data", date_time = "date",
          year = "year", rain = "rain",
          start_day = 121, end_day = 300)
daily_data_by_year <- data_book$get_data_frame("daily_data_by_year")

expected_station_year_results <- readRDS("testdata/end_rains_by_station_year.rds")
expected_year_results <- readRDS("testdata/end_rains_by_year.rds")

test_that("Correct summaries are calculated", {
  expect_true(identical(expected_station_year_results, daily_data_by_station_name_year))
  expect_true(identical(expected_year_results, daily_data_by_year))
})
