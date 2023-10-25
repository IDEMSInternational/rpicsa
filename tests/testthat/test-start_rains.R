library(rpicsa)
library(dplyr)
library(testthat)

# Test case 1
daily_data <- rpicsa::daily_niger %>%
  filter(year <= 1950) %>%
  filter(year > 1945) %>%
  filter(station_name == "Agades")
test_1_results <- readRDS("data/test_1_annual_summaries.rds") %>%
  dplyr::select(c(year, station_name, start_rains)) %>%
  dplyr::arrange(year)

test_that("Correct summaries are calculated", {
  result <- start_rains(data = daily_data, date_time = "date", station = "station_name",
                        year = "year", doy = "doy", rain = "rain",
                        total_rainfall = TRUE, over_days = 3, amount_rain = 25,
                        dry_spell = TRUE, spell_interval = 21, spell_max_dry_days = 9
  )
  result <- full_join(result, test_1_results, by = c("station_name" = "station_name",
                                           "year" = "year"))
  expect_true(identical(result$start_rains.x, result$start_rains.y))
})