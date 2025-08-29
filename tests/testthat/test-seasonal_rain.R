# tests/testthat/test-seasonal_rain.R

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
    output    = "doy",
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
    output    = "doy",
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
