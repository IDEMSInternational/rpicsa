library(rpicsa)
library(cdms.products) # for niger data
library(dplyr)

# Testing annual_rain function --------------------
daily_niger_1 <- daily_niger %>% dplyr::filter(year == 1951) %>% dplyr::filter(station_name == "Agades")

rain_seasonal <- seasonal_rain(data = daily_niger_1, station = "station_name", date_time = "date",
                               year = "year", doy = "doy", rain = "rain", end_type = "rains") %>% dplyr::pull(total_rain)

rain_seasonal_1 <- daily_niger %>%
  dplyr::filter(date >= as.Date("1951-08-18")) %>%
  dplyr::filter(date <= as.Date("1951-09-17")) %>%
  dplyr::filter(station_name == "Agades") %>%
  dplyr::summarise(total_rain = sum(rain)) %>%
  dplyr::pull(total_rain)

daily_niger_2 <- daily_niger %>% dplyr::filter(year <= 1951) %>% dplyr::filter(station_name == "Agades")
start_output <- start_rains(data = daily_niger_2, station = "station_name", date_time = "date",
                            year = "year", doy = "doy", rain = "rain")
end_output <- end_rains(data = daily_niger_2, station = "station_name", date_time = "date",
                        year = "year", doy = "doy", rain = "rain")
summary_data <- dplyr::full_join(start_output, end_output)
start_output_1 <- seasonal_rain(summary_data = summary_data, date_time = "date", station = "station_name", data = daily_niger_2, year = "year", start_date = "start_rain", end_date = "end_rain", rain = "rain", end_type = "rains")
start_output_2 <- seasonal_rain(summary_data = summary_data, date_time = "date", station = "station_name", data = daily_niger_2, year = "year", rain = "rain", end_type = "rains")


test_that("Returns correct annual totals for one summary", {
  expect_equal(rain_seasonal_1, rain_seasonal)
})

test_that("Returns same data frame regardless of if the function calculates SOR/EOR", {
  expect_equal(start_output_1, start_output_2)
})

