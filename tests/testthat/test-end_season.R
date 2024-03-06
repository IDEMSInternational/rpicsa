library(rpicsa)
library(dplyr)
library(testthat)

# Test case 1
daily_data <- rpicsa::daily_niger %>%
  filter(year <= 1950) %>%
  filter(year > 1945) %>%
  filter(station_name == "Agades")
test_1_results <- readRDS("testdata/test_1_annual_summaries.rds") %>%
  dplyr::select(c(year, station_name, end_season)) %>%
  dplyr::mutate(year = factor(year)) %>%
  dplyr::arrange(year)

test_that("Correct summaries are calculated", {
  result <- end_season(data = daily_data, date_time = "date", station = "station_name",
                      year = "year", doy = "doy", rain = "rain",
                      start_day = 121)
  result <- full_join(result, test_1_results, by = c("station_name" = "station_name",
                                                     "year" = "year"))
  expect_true(identical(result$end_season.x, result$end_season.y))
})