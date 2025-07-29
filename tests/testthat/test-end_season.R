test_that("Correct summaries are calculated", {
  # Testing Load packages --------------------
  library(databook)
  expected_station_year_results <- readRDS("testdata/end_season_by_station_year.rds")
  expected_year_results <- readRDS("testdata/end_season_by_year.rds")
  expected_year_no_reducing_results <- readRDS("testdata/end_season_by_year_no_reducing.rds")
  
  # Setting up R Code --------------------
  data_book <- DataBook$new()
  daily_data <- rpicsa::daily_niger %>%
    dplyr::filter(year <= 1950) %>%
    dplyr::filter(year > 1945) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::filter(station_name == "Agades") %>%
    dplyr::mutate(evap_var = 5)
  data_book$import_data(list(daily_data = daily_data))
  data_book$add_key("daily_data", "date", "key")
  
  daily_data <- data_book$get_data_frame("daily_data")
  suppressWarnings(end_season(data = "daily_data",
             date_time = "date",
             station = "station_name",
             rain = "rain",
             start_day = 121,
             end_day = 300,
             data_book = data_book))
  daily_data_by_station_name_year <- data_book$get_data_frame("daily_data_by_station_name_year")
  
  suppressWarnings(end_season(data = "daily_data",
                              date_time = "date",
                              rain = "rain",
                              year = "year",
                              start_day = 121,
                              end_day = 300,
                              evaporation  = "variable",
                              evaporation_variable = "evap_var",
                              reducing = TRUE,
                              data_book = data_book))
  daily_data_by_year <- data_book$get_data_frame("daily_data_by_year")
  
  suppressWarnings(end_season(data = "daily_data",
                              date_time = "date",
                              rain = "rain",
                              year = "year",
                              start_day = 121,
                              end_day = 300,
                              evaporation  = "variable",
                              evaporation_variable = "evap_var",
                              reducing = FALSE,
                              data_book = data_book))
  daily_data_by_year_no_reducing <- data_book$get_data_frame("daily_data_by_year")
  
  # saveRDS(daily_data_by_station_name_year, "testdata/end_season_by_station_year.rds")

  expect_identical(expected_station_year_results, daily_data_by_station_name_year)
  expect_identical(expected_year_results, daily_data_by_year)
  expect_identical(expected_year_no_reducing_results, daily_data_by_year_no_reducing)
})
