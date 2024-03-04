library(rpicsa)
library(testthat)
library(dplyr)

# Sample data for testing
test_data <- data.frame(
  date = seq(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "day"),
  rain = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
  temp = c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
)

# Test for threshold type
test_that("Test threshold type", {
  result <- get_extremes(test_data, "rain", "threshold", 50)
  expect_true(all(result$rain > 50))
})

# Test for percentile type
test_that("Test percentile type", {
  result <- get_extremes(test_data, "temp", "percentile", 90)
  expect_true(all(result$temp > quantile(test_data$temp, probs = 0.90)))
})

# Test for direction 'less'
test_that("Test direction 'less'", {
  result <- get_extremes(test_data, "temp", "threshold", 20, "less")
  expect_true(all(result$temp < 20))
})

# Test for error handling
test_that("Test error handling for non-existent column", {
  expect_error(get_extremes(test_data, "non_existent_column", "threshold", 50))
})
