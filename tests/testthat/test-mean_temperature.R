test_that("Returns correct yearly summaries", {
  library(databook)
  
  # Setting up data to compare to --------------------
  niger <- daily_niger %>%
    dplyr::filter(year %in% 1950:1951)
  
  x_year <- niger %>% 
    dplyr::group_by(station_name, year) %>% 
    dplyr::summarise(mean_tmin = mean(tmin),
                     min_tmin = min(tmin),
                     max_tmin = max(tmin)
    )
  
  x_month <- niger %>% 
    dplyr::group_by(station_name, month) %>% 
    dplyr::summarise(mean_tmin = mean(tmin),
                     min_tmin = min(tmin),
                     max_tmin = max(tmin)
    )
  
  # Setting up what our function gives ------------------------
  data_book <- DataBook$new()
  data_book$import_data(list(niger = niger))
  
  summary_temperature(
    data      = "niger",
    date_time = "date",
    station   = "station_name",
    tmin      = "tmin",
    data_book = data_book
  )
  y_year <- data_book$get_data_frame("niger_by_station_name_year")
  
  y_month <- summary_temperature(data = "niger",
                                 year = "year",
                                 date_time = "date",
                                 station = "station_name",
                                 tmin = "tmin",
                                 data_book = data_book)
  y_month <- data_book$get_data_frame("niger_by_station_name_month")
  
  expect_length(y_month, 5)
  expect_equal(as.numeric(x_year$mean_tmin - y_year$mean_tmin), c(rep(0, 8)))
  
  expect_length(y_year, 5)
  expect_equal(as.numeric(x_month$mean_tmin - y_month$mean_tmin), c(rep(0, 48)))
  
  expect_error(summary_temperature(data = "niger",
                                   year = "year",
                                   date_time = "date",
                                   station = "station_name",
                                   data_book = data_book),
               "At least one of 'tmin' or 'tmax' must be provided.")
})
