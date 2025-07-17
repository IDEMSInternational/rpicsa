library(rpicsa)
library(dplyr)
library(databook)

# Testing annual_rain function --------------------
data_book <- DataBook$new()
niger <- daily_niger %>%
  filter(year %in% 1950:1951)
data_book$import_data(list(niger = niger))

x_sum <- niger %>% 
  group_by(year, station_name) %>% 
  summarise(sum_rain = sum(rain))

x_both <- niger %>% 
  group_by(year, station_name) %>% 
  summarise(annual_rain = sum(rain),
            n_rain = sum(rain > 0.85))

y_sum <- annual_rain(data = "niger", year = "year", n_rain = FALSE,
                     station = "station_name", rain = "rain")
y_sum <- data_book$get_data_frame("niger_by_year_station_name")

y_both <- annual_rain(data = "niger", rain_day = 0.85,
                     station = "station_name", year = "year", rain = "rain")
y_both <- data_book$get_data_frame("niger_by_year_station_name")

test_that("Returns correct annual totals for one summary", {
  expect_length(y_sum, 3)
  expect_equal(as.numeric(x_sum$sum_rain - y_sum$sum_rain), c(rep(0, 8)))
  expect_setequal(colnames(x_sum), colnames(y_sum))
})

test_that("Returns correct annual totals for both summaries", {
  expect_length(x_both, 4)
  expect_equal(as.numeric(x_both$annual_rain - y_both$sum_rain), c(rep(0, 8)))
  expect_equal(as.numeric(x_both$n_rain - y_both$sum_rainfall_count ), c(rep(0, 8)))
})

