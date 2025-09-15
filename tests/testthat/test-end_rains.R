test_that("Correct summaries are calculated", {
  # Testing Load packages --------------------
  library(databook)
  
  # Setting up R Code --------------------
  data_book <- DataBook$new()
  daily_data <- rpicsa::daily_niger %>%
    dplyr::filter(year <= 1950) %>%
    dplyr::filter(year > 1945) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::filter(station_name == "Agades")
  data_book$import_data(list(daily_data = daily_data))
  
  daily_data <- data_book$get_data_frame("daily_data")
  
  suppressWarnings(end_rains(data = "daily_data",
                             date_time = "date",
                             station = "station_name",
                             rain = "rain",
                             start_day = 121,
                             end_day = 300,
                             data_book = data_book))
  daily_data_by_station_name_year <- data_book$get_data_frame("daily_data_by_station_name_year")
  
  suppressWarnings(end_rains(data = "daily_data",
                             date_time = "date",
                             year = "year",
                             rain = "rain",
                             start_day = 121,
                             end_day = 300,
                             data_book = data_book))
  daily_data_by_year <- data_book$get_data_frame("daily_data_by_year")
  
  expected_station_year_results <- readRDS("testdata/end_rains_by_station_year.rds")
  expected_year_results <- readRDS("testdata/end_rains_by_year.rds")
  
  expect_true(identical(expected_station_year_results, daily_data_by_station_name_year))
  expect_true(identical(expected_year_results, daily_data_by_year))
})
