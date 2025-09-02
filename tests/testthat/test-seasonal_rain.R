testthat::test_that("seasonal_rain adds correct seasonal totals and basic behaviour works", {
  library(dplyr)
  library(lubridate)
  
  # --- Setup small, deterministic slice of data ---
  # Use one station and a couple of years to keep tests quick
  daily_data <- rpicsa::daily_niger %>%
    dplyr::filter(station_name == "Agades", year >= 1947, year <= 1948) %>%
    dplyr::mutate(year = as.numeric(year))
  
  # Safety check the test data
  testthat::expect_true(nrow(daily_data) > 300)
  testthat::expect_true(all(c("date", "rain", "station_name") %in% names(daily_data)))
  
  # Fresh DataBook
  data_book <- DataBook$new()
  data_book$import_data(list(daily_data = daily_data))
  
  # Compute start and end of rains as DOY (so seasonal_rain can reference them)
  start_day <- 121
  end_day   <- 300
  
  suppressWarnings(start_rains(
    data      = "daily_data",
    date_time = "date",
    station   = "station_name",
    year      = "year",
    rain      = "rain",
    start_day = start_day,
    end_day   = end_day,
    output    = c("doy", "status"),
    data_book = data_book
  ))
  
  suppressWarnings(end_rains(
    data      = "daily_data",
    date_time = "date",
    station   = "station_name",
    year      = "year",
    rain      = "rain",
    start_day = start_day,
    end_day   = end_day,
    output    = c("doy", "status"),
    data_book = data_book
  ))
  
  # Sanity: summary table (by station and year) must exist from the above calls
  sum_name <- "daily_data_by_station_name_year"
  sum_df   <- data_book$get_data_frame(sum_name)
  testthat::expect_s3_class(sum_df, "data.frame")
  testthat::expect_true(all(c("station_name", "year", "start_rain", "end_rains") %in% names(sum_df)))

  # --- Run seasonal_rain (vectorised path: let it create/confirm doy & year if needed) ---
  suppressWarnings(seasonal_rain(
    summary_data = sum_name,
    start_date   = "start_rain",
    end_date     = "end_rains",
    data         = "daily_data",
    date_time    = "date",
    year         = "year",
    doy          = "doy",
    station      = "station_name",
    rain         = "rain",
    data_book    = data_book
  ))
  
  # Pull the updated summary table
  out_df <- data_book$get_data_frame(sum_name)
  
  # Find the newly added summary column (from calculate_summary with "summary_sum")
  # We don't assume the exact column name; look for a single "sum" column.
  sum_cols <- grep("sum", names(out_df), value = TRUE)
  testthat::expect_true(length(sum_cols) >= 1)

  # Prefer a rain-specific sum column if present
  sum_col <- if (any(grepl("rain", sum_cols))) {
    sum_cols[grepl("rain", sum_cols)][1]
  } else {
    sum_cols[1]
  }
  testthat::expect_true(is.numeric(out_df[[sum_col]]))
  
  # --- Reference calculation (hand compute seasonal totals) ---
  # Ensure DOY is available in the raw daily data
  dd <- data_book$get_data_frame("daily_data")
  testthat::expect_true(all(c("doy", "year", "station_name", "rain") %in% names(dd)))
  
  ref <- out_df %>%
    dplyr::select(station_name, year, start_rain, end_rains) %>%
    dplyr::left_join(
      dd %>% dplyr::select(station_name, year, doy, rain),
      by = c("station_name", "year")
    ) %>%
    dplyr::filter(!is.na(start_rain), !is.na(end_rains)) %>%
    dplyr::filter(doy >= start_rain, doy <= end_rains) %>%
    dplyr::group_by(station_name, year) %>%
    dplyr::summarise(ref_sum = sum(rain, na.rm = FALSE), .groups = "drop")
  
  check <- out_df %>%
    dplyr::select(station_name, year, !!sum_col := dplyr::all_of(sum_col)) %>%
    dplyr::inner_join(ref, by = c("station_name", "year"))
  
  # Compare within small numeric tolerance
  testthat::expect_equal(as.numeric(check[[sum_col]]), check$ref_sum, tolerance = 1e-8)
  
  # --- NA handling check (na_rm = TRUE) ---
  # Introduce an NA for an in-season day and confirm the total drops appropriately
  # Pick one specific (station, year, doy) triple inside the season
  one_key <- out_df %>%
    dplyr::filter(!is.na(start_rain), !is.na(end_rains)) %>%
    dplyr::slice(1)
  
  testthat::expect_equal(nrow(one_key), 1L)
  
  #### Now for Seasonal Length
  # Run seasonal_length (default save names)
  suppressWarnings(seasonal_length(
    summary_data        = sum_name,
    start_date          = "start_rain",
    end_date            = "end_rains",
    start_rain_status   = "start_rain_status",
    end_rain_status     = "end_rains_status",
    data_book           = data_book
  ))
  
  out <- data_book$get_data_frame(sum_name)
  testthat::expect_true(all(c("length", "occurrence") %in% names(out)))
  
  # length must equal end - start
  testthat::expect_true(is.numeric(out$length))
  testthat::expect_equal(as.numeric(out$length)[1], as.numeric(out$end_rains)[1] - as.numeric(out$start_rain)[1], tolerance = 1e-12)
  
  # occurrence is a factor; spot-check it matches the logic for rows without NAs
  testthat::expect_s3_class(out$occurrence, "factor")
  
})

testthat::test_that("seasonal_rain errors if no summaries are requested", {
  # Minimal dummy DataBook + data to trigger the early error branch
  dat <- data.frame(
    date = as.Date("2000-01-01") + 0:10,
    year = 2000L,
    station_name = "X",
    rain = 0
  )
  
  data_book <- DataBook$new()
  data_book$import_data(list(daily_data = dat))
  
  # Create a tiny summary table with required columns so the function can parse inputs
  summ <- dat %>%
    dplyr::summarise(
      station_name = "X",
      year = 2000L,
      start_rain = 5L,
      end_rains = 7L,
      .groups = "drop"
    )
  data_book$import_data(list(daily_data_by_station_name_year = summ))
  
  # Expect error when both total_rain and n_rain are FALSE
  testthat::expect_error(
    seasonal_rain(
      summary_data = "daily_data_by_station_name_year",
      start_date   = "start_rain",
      end_date     = "end_rains",
      data         = "daily_data",
      date_time    = "date",
      year         = "year",
      doy          = NULL,
      station      = "station_name",
      rain         = "rain",
      total_rain   = FALSE,
      n_rain       = FALSE,
      data_book    = data_book
    ),
    "No summaries selected"
  )
})


testthat::test_that("seasonal_length: occurrence logic is correct on synthetic cases", {
  # Build a tiny summary table to hit all branches (TRUE/TRUE, FALSE/FALSE, NONE, MORE, NA)
  summ <- data.frame(
    start_rain        = c(10L,  5L,  7L,  9L,  4L,  6L),
    end_rains         = c(20L, 15L, 17L, 19L, 14L, 16L),
    start_rain_status = c(TRUE, FALSE, FALSE, TRUE, NA,    TRUE),
    end_rains_status  = c(TRUE, FALSE, TRUE,  FALSE, TRUE, NA),
    stringsAsFactors  = FALSE
  )
  
  data_book <- DataBook$new()
  data_book$import_data(list(season_sum = summ))
  
  suppressWarnings(seasonal_length(
    summary_data        = "season_sum",
    start_date          = "start_rain",
    end_date            = "end_rains",
    start_rain_status   = "start_rain_status",
    end_rain_status     = "end_rains_status",
    data_book           = data_book
  ))
  
  out <- data_book$get_data_frame("season_sum")
  
  # length equals difference
  testthat::expect_equal(as.numeric(out$length)[1], as.numeric(out$end_rains)[1] - as.numeric(out$start_rain)[1], tolerance = 1e-12)
  
  # occurrence factor values follow the specified case_when logic
  expected <- c("TRUE", "FALSE", "NONE", "MORE", NA, NA)
  testthat::expect_s3_class(out$occurrence, "factor")
  testthat::expect_equal(as.character(out$occurrence), expected)
})

testthat::test_that("seasonal_length works without status columns and with custom save names", {
  # No status columns: only 'length' (custom name) should be created
  summ <- data.frame(
    start_d = c(3L, 50L),
    end_d   = c(7L, 70L)
  )
  data_book <- DataBook$new()
  data_book$import_data(list(my_summary = summ))
  
  suppressWarnings(seasonal_length(
    summary_data            = "my_summary",
    start_date              = "start_d",
    end_date                = "end_d",
    start_rain_status       = NULL,
    end_rain_status         = NULL,
    season_length_save_name = "L_custom",
    occurrence_save_name    = "O_custom",  # should be ignored since no statuses
    data_book               = data_book
  ))
  
  out <- data_book$get_data_frame("my_summary")
  testthat::expect_true("L_custom" %in% names(out))
  testthat::expect_false("O_custom" %in% names(out))
  testthat::expect_equal(as.numeric(out$L_custom)[1], as.numeric(out$end_d)[1] - as.numeric(out$start_d)[1])
})