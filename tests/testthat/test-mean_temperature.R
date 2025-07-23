library(rpicsa)
library(dplyr)
library(databook)

# Testing annual_rain function --------------------
data_book <- DataBook$new()
niger <- daily_niger %>%
  filter(year %in% 1950:1951)
data_book$import_data(list(niger = niger))

x_year <- niger %>% 
  group_by(station_name, year) %>% 
  summarise(mean_tmin = mean(tmin),
            min_tmin = min(tmin),
            max_tmin = max(tmin)
            )

x_month <- niger %>% 
  group_by(station_name, month) %>% 
  summarise(mean_tmin = mean(tmin),
            min_tmin = min(tmin),
            max_tmin = max(tmin)
  )
y_year <- summary_temperature(data = "niger", year = "year",
                     station = "station_name", tmin = "tmin",
                      to = "annual")
y_year <- data_book$get_data_frame("niger_by_station_name_year")

y_month <- summary_temperature(data = "niger", year = "year", month  = "month",
                             station = "station_name", tmin = "tmin",
                             to = "monthly")
y_month <- data_book$get_data_frame("niger_by_station_name_month")

test_that("Returns correct yearly summaries", {
  expect_length(y_month, 5)
  
  expect_equal(as.numeric(x_year$mean_tmin - y_year$mean_tmin), c(rep(0, 8)))
})

test_that("Returns correct monthly summaries", {
  expect_length(y_year, 5)
  expect_equal(as.numeric(x_month$mean_tmin - y_month$mean_tmin), c(rep(0, 48)))
})
