

testthat::test_that("seasonal_length works without status columns and with custom save names", {
  # No status columns: only 'length' (custom name) should be created
  summ <- data.frame(
    start_d = c(3L, 50L),
    end_d   = c(7L, 70L)
  )
  data_book <- DataBook$new()
  data_book$import_data(list(my_summary = summ))
  
  seasonal_length(
    summary_data            = "my_summary",
    start_date              = "start_d",
    end_date                = "end_d",
    start_rain_status       = NULL,
    end_rain_status         = NULL,
    season_length_save_name = "L_custom",
    occurrence_save_name    = "O_custom",  # should be ignored since no statuses
    data_book               = data_book
  )
  
  out <- data_book$get_data_frame("my_summary")
  testthat::expect_true("L_custom" %in% names(out))
  testthat::expect_false("O_custom" %in% names(out))
  testthat::expect_equal(out$L_custom, out$end_d - out$start_d)
})
