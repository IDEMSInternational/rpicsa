make_daily <- function(n_days = 30, start_date = as.Date("2000-01-01"),
                       station_levels = NULL, seed = 1) {
  set.seed(seed)
  dates <- seq(start_date, by = "1 day", length.out = n_days)
  base <- data.frame(
    date = dates,
    # create some dry/wet streaks: mostly 0 with occasional > 1
    rain = as.numeric(rbinom(n_days, 1, prob = 0.3)) * runif(n_days, 1, 5),
    stringsAsFactors = FALSE
  )
  if (!is.null(station_levels)) {
    base$station_name <- factor(sample(station_levels, n_days, replace = TRUE),
                                levels = station_levels)
  }
  base
}

testthat::test_that("Creates year/doy when not provided, no station", {
  library(databook)
  
  db <- DataBook$new()
  df <- make_daily(n_days = 40)
  db$import_data(list(daily_data = df))
  
  # max spell in full range, threshold less than or equal 0 => dry days
  get_spells_data(
    data = "daily_data",
    date_time = "date",
    year = NULL,
    station = NULL,
    element = "rain",
    doy = NULL,
    # numeric day window:
    day_from = 1, day_to = 366,
    direction = "less", value = 0,  # spell of zeros
    return_max_spell = TRUE,
    return_all_spells = FALSE,
    data_book = db
  )
  
  # By default summary should be saved in a new df (from your code: save = 2)
  # We can't rely on an exact name in all cases, but we can check that at least one new df appears:
  dfs <- db$get_data_names()
  testthat::expect_true(length(dfs) == 2) # original + at least one result
  
  # sanity: year/doy columns should have been created in the original
  got <- db$get_data_frame("daily_data")
  testthat::expect_true(all(c("year", "doy") %in% names(got)))
})

testthat::test_that("Works with station grouping (station not NULL)", {
  library(databook)
  
  db <- DataBook$new()
  df <- make_daily(n_days = 60, station_levels = c("A", "B"))
  db$import_data(list(daily_data = df))
  
  get_spells_data(
    data = "daily_data",
    date_time = "date",
    year = NULL,
    station = "station_name",
    element = "rain",
    doy = NULL,
    day_from = 1, day_to = 366,
    direction = "less", value = 0,
    return_max_spell = TRUE,
    return_all_spells = FALSE,
    data_book = db
  )
  
  # Look for a grouped summary output (name depends on instatCalculations rules)
  dfs <- db$get_data_names()
  # Expect at least original plus one or more result frames
  testthat::expect_true(length(dfs) >= 2)
  
  # Check that some result frame has both station and year (common in grouping)
  found_grouped <- FALSE
  for (nm in dfs) {
    res <- tryCatch(db$get_data_frame(nm), error = function(e) NULL)
    if (!is.null(res) && all(c("year", "station_name") %in% names(res))) {
      found_grouped <- TRUE
      # Max spell must be non-negative integer(ish)
      if ("spells" %in% names(res)) {
        testthat::expect_true(all(res$spells >= 0, na.rm = TRUE))
      }
    }
  }
  testthat::expect_true(found_grouped)
})

testthat::test_that("return_all_spells produces spells_filter frame", {
  library(databook)
  
  db <- DataBook$new()
  df <- make_daily(n_days = 35, station_levels = c("A"))
  db$import_data(list(daily_data = df))
  
  get_spells_data(
    data = "daily_data",
    date_time = "date",
    station = "station_name",
    element = "rain",
    day_from = 1, day_to = 366,
    direction = "less", value = 0,
    return_max_spell = FALSE,
    return_all_spells = TRUE,
    data_book = db
  )
  
  # The code uses result_data_frame = "spells_filter"
  testthat::expect_true("spells_filter" %in% db$get_data_names())
  sf <- db$get_data_frame("spells_filter")
  # Expect columns like spell_length and (often) year/station_name
  testthat::expect_true("spell_length" %in% names(sf))
  testthat::expect_true(all(sf$spell_length >= 0, na.rm = TRUE))
})

testthat::test_that("Errors & validation: missing columns and bad bounds", {
  library(databook)
  
  db <- DataBook$new()
  df <- data.frame(
    date = as.Date("2002-01-01") + 0:9,
    rain = runif(10)
  )
  db$import_data(list(daily_data = df))
  
  # Missing element column should error
  testthat::expect_error(
    get_spells_data(
      data = "daily_data",
      date_time = "date",
      element = "not_a_col",
      direction = "less", value = 0,
      data_book = db
    ),
    regexp = "not_a_col|assert|column", ignore.case = TRUE
  )
  
  # Bad numeric bounds should error
  testthat::expect_error(
    get_spells_data(
      data = "daily_data",
      date_time = "date",
      element = "rain",
      day_from = 0, day_to = 400,  # invalid
      direction = "less", value = 0,
      data_book = db
    ),
    regexp = "1.*366|assert", ignore.case = TRUE
  )
})
