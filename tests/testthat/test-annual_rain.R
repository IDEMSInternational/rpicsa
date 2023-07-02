library(rpicsa)
library(cdms.products) # for niger data
library(dplyr)

# Testing annual_rain function --------------------

niger <- daily_niger %>%
  filter(year %in% 1950:1951)

x_sum <- niger %>% 
  group_by(station_name, year) %>% 
  summarise(annual_rain = sum(rain))

x_both <- niger %>% 
  group_by(station_name, year) %>% 
  summarise(annual_rain = sum(rain),
            n_rain = sum(rain > 0.85))

y_sum <- annual_rain(data = niger, date_time = "date", n_rain = FALSE,
                     station = "station_name", rain = "rain")

y_both <- annual_rain(data = niger, date_time = "date", rain_day = 0.85,
                     station = "station_name", rain = "rain")

test_that("Returns correct annual totals for one summary", {
  expect_length(y_sum, 3)
  expect_equal(x_sum, y_sum)
  expect_setequal(colnames(x_sum), colnames(y_sum))
})

test_that("Returns correct annual totals for both summaries", {
  expect_length(x_both, 4)
  expect_equal(x_both, y_both)
  expect_setequal(colnames(x_both), colnames(y_both))
})

