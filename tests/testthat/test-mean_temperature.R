library(rpicsa)
library(cdms.products) # for niger data
library(dplyr)

# Testing mean_temperature function --------------------

niger <- daily_niger %>%
  filter(year %in% 1950:1951)

x_monthly <- niger %>% 
  group_by(station_name, year, month) %>% 
  summarise(mean_tmin = mean(tmin), mean_tmax = mean(tmax))

x_annual <- niger %>% 
  group_by(station_name, year) %>% 
  summarise(mean_tmin = mean(tmin), mean_tmax = mean(tmax))

y_monthly <- mean_temperature(data = niger, date_time = "date", month = "month",
                              station = "station_name", to = "monthly", tmin = "tmin",
                              tmax = "tmax")

y_annual <- mean_temperature(data = niger, date_time = "date",
                             station = "station_name", tmin = "tmin",
                             tmax = "tmax")

x_tmin_monthly <- niger %>% 
  group_by(station_name, year, month) %>% 
  summarise(mean_tmin = mean(tmin))

x_tmin_annual <- niger %>% 
  group_by(station_name, year) %>% 
  summarise(mean_tmin = mean(tmin))

y_tmin_monthly <- mean_temperature(data = niger, date_time = "date", month = "month",
                              station = "station_name", to = "monthly", tmin = "tmin")

y_tmin_annual <- mean_temperature(data = niger, date_time = "date",
                             station = "station_name", tmin = "tmin")

test_that("Returns correct monthly summaries", {
  expect_length(y_monthly, 5)
  expect_equal(x_monthly, y_monthly)
  expect_setequal(colnames(x_monthly), colnames(y_monthly))
})

test_that("Returns correct annual summaries", {
  expect_length(y_annual, 4)
  expect_equal(x_annual, y_annual)
  expect_setequal(colnames(x_annual), colnames(y_annual))
})

test_that("Returns correct monthly summaries for just one element", {
  expect_length(y_tmin_monthly, 4)
  expect_equal(x_tmin_monthly, y_tmin_monthly)
  expect_setequal(colnames(x_tmin_monthly), colnames(y_tmin_monthly))
})

test_that("Returns correct annual summaries for just one element", {
  expect_length(y_tmin_annual, 3)
  expect_equal(x_tmin_annual, y_tmin_annual)
  expect_setequal(colnames(x_tmin_annual), colnames(y_tmin_annual))
})

