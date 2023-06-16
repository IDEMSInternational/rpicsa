library(rpicsa)
library(cdms.products) # for niger data
library(dplyr)

data(daily_niger)

daily_niger_1 <- daily_niger %>% filter(year > 1944)
daily_niger_1 <- daily_niger_1 %>% filter(year < 1950)

start_output <- start_rains(data = daily_niger_1, station = "station_name", date_time = "date",
                            year = "year", doy = "doy", rain = "rain")
end_output_rain <- end_rains(data = daily_niger_1, station = "station_name", date_time = "date",
                             year = "year", doy = "doy", rain = "rain")
end_output_season <- end_season(data = daily_niger_1, station = "station_name", date_time = "date",
                                year = "year", doy = "doy", rain = "rain")
summary_data_rain <- dplyr::full_join(start_output, end_output_rain) %>%
  dplyr::arrange(station_name, year)
summary_data_season <- dplyr::full_join(start_output, end_output_season) %>%
  dplyr::arrange(station_name, year)

# end rain calculated beforehand
output_1 <- seasonal_length(summary_data = summary_data_rain, start_date = "start_rain", end_date = "end_rain")$season_length

# end season calculated beforehand
output_2 <- seasonal_length(summary_data = summary_data_season, start_date = "start_rain", end_date = "end_season")$season_length

# end rain calculated in the function
output_1_1 <- seasonal_length(summary_data = start_output, start_date = "start_rain",
                              data = daily_niger_1, station = "station_name",
                              end_type = "rains", date_time = "date",
                              year = "year", doy = "doy", rain = "rain") %>%
  dplyr::arrange(station_name, year) %>%
  dplyr::pull(season_length)

# end season calculated in the function
output_2_1 <- seasonal_length(summary_data = start_output, start_date = "start_rain",
                              data = daily_niger_1, station = "station_name",
                              end_type = "season", date_time = "date",
                              year = "year", doy = "doy", rain = "rain") %>%
  dplyr::arrange(station_name, year) %>%
  dplyr::pull(season_length)

# start rain calculted in function
start_output_1 <- seasonal_length(summary_data = end_output_rain, end_date = "end_rain",
                                  data = daily_niger_1, station = "station_name", date_time = "date",
                                  year = "year", doy = "doy", rain = "rain")$season_length

start_output_2 <- seasonal_length(summary_data = end_output_season, end_date = "end_season",
                                  data = daily_niger_1, station = "station_name", date_time = "date",
                                  year = "year", doy = "doy", rain = "rain")$season_length

summary_data_all <- dplyr::full_join(summary_data_rain, summary_data_season)
all_output_give <- seasonal_length(summary_data = summary_data_all, start_date = "start_rain", end_date = "end_rain") %>%
  dplyr::arrange(station_name, year) %>% dplyr::pull(season_length)
all_output_calc <- seasonal_length(data = daily_niger_1, station = "station_name", date_time = "date",
                                  year = "year", doy = "doy", rain = "rain", end_type = "rains") %>%
  dplyr::arrange(station_name, year) %>% dplyr::pull(season_length)

test_that("end rain / start rain gives same result regardless if calculated before or not", {
  expect_equal(output_1, output_1_1)
  expect_equal(output_1, start_output_1)
})

test_that("end season / start rain gives same result regardless if calculated before or not", {
  expect_equal(output_2, output_2_1)
  expect_equal(output_2, start_output_2)
})

test_that("same result regardless if all calculated before or not", {
  expect_equal(all_output_give, all_output_calc)
})
