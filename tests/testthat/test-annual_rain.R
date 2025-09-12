test_that("Returns correct annual totals for one summary", {
  library(databook)
  
  # Testing annual_rain function --------------------
  data_book <- DataBook$new()
  niger <- daily_niger %>%
    dplyr::filter(year %in% 1950:1951)
  data_book$import_data(list(niger = niger))
  
  x_sum <- niger %>% 
    dplyr::group_by(station_name, year) %>% 
    dplyr::summarise(sum_rain = sum(rain))
  
  x_both <- niger %>% 
    dplyr::group_by(station_name, year) %>% 
    dplyr::summarise(annual_rain = sum(rain),
                     n_rain = sum(rain > 0.85))
  
  y_sum <- suppressWarnings(annual_rain(data = "niger",
                                        year = "year",
                                        n_rain = FALSE,
                                        station = "station_name",
                                        rain = "rain",
                                        data_book = data_book))
  y_sum <- data_book$get_data_frame("niger_by_station_name_year")
  
  y_both <- suppressWarnings(annual_rain(data = "niger",
                                         rain_day = 0.85,
                                         station = "station_name",
                                         year = "year",
                                         rain = "rain",
                                         data_book = data_book))
  y_both <- data_book$get_data_frame("niger_by_station_name_year")
  
  expect_length(y_sum, 3)
  expect_equal(as.numeric(x_sum$sum_rain - y_sum$sum_rain), c(rep(0, 8)))
  expect_setequal(colnames(x_sum), colnames(y_sum))
  expect_length(x_both, 4)
  expect_equal(as.numeric(x_both$annual_rain - y_both$sum_rain), c(rep(0, 8)))
  expect_equal(as.numeric(x_both$n_rain - y_both$sum_rainfall_count ), c(rep(0, 8)))
  
  expect_error(annual_rain(data = "niger",
                           rain_day = 0.85,
                           station = "station_name",
                           year = "year",
                           rain = "rain",
                           data_book = data_book,
                           total_rain = FALSE,
                           n_rain = FALSE),
               "No summaries selected. At least one of\n         'total_rain' or 'n_rain' must be TRUE.")
})
