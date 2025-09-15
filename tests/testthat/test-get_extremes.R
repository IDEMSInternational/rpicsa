testthat::test_that("seasonal_rain adds correct seasonal totals and basic behaviour works", {
  library(databook)
  data_book <- DataBook$new()
  daily_data <- rpicsa::daily_niger |>
    dplyr::filter(year > 1945, year <= 1955, station_name == "Agades") |>
    dplyr::mutate(year = as.numeric(year))
  data_book$import_data(list(daily_data = daily_data))
  
  # Looking at the number of instances in a year that rainfall exceeds 40mm in a day
  get_extremes(data = "daily_data",
               date_time  = "date",
               station = "station_name",
               element = "rain",
               direction = "greater",
               na_rm = TRUE,
               na_n = 10,
               value = 40,
               data_book = data_book)
  
  # The extreme days are under "sum_extreme_rain" here:
  returned_data <- data_book$get_data_frame("daily_data_by_station_name_year")
  
  expect_equal(ncol(returned_data), 3)
  expect_equal(as.numeric(returned_data$sum_extreme_rain), 
               c(0, 0, 0, 0, 2, 0, 0, 0, 1, 0))
  
  
})