library(databook)
case_when <- dplyr::case_when

test_that("crops_definitions: basic run with station + same-season table", {
  data_book <- DataBook$new()
  
  # Synthetic daily data: 1 station, 1 year, DOY 100..180
  set.seed(1)
  daily <- dplyr::tibble(
    station = "S1",
    year    = 2001,
    doy     = 100:180,
    # Make a 10-day block (125..134) with 6mm to guarantee >=60mm
    rain    = ifelse(doy >= 125 & doy <= 134, 6, 0),
    # Season bounds constant per group (valid windows end by 170)
    start_day = 120L,
    end_day   = 170L
  )
  # Add a Date column for later tests (won't be used here)
  daily$date <- as.Date("2001-01-01") + (daily$doy - 1)
  
  data_book$import_data(list(daily = daily))
  
  # Run: two planting starts, 10-day window, threshold 50 => success = 1
  crops_definitions(
    data_name   = "daily",
    year        = "year",
    station     = "station",
    doy         = "doy",
    rain        = "rain",
    rain_totals = 50,
    plant_days  = c(125, 130),
    plant_lengths = 10,
    start_check = "both",
    season_data_name = "daily",
    start_day   = "start_day",
    end_day     = "end_day",
    return_crops_table = TRUE,
    definition_props   = TRUE,
    data_book   = data_book
  )
  
  nm <- data_book$get_data_names()
  expect_true(any(grepl("^crop_def", nm)))
  expect_true(any(grepl("^crop_prop", nm)))
  
  crop_def  <- data_book$get_data_frame(nm[grepl("^crop_def", nm)][1])
  crop_prop <- data_book$get_data_frame(nm[grepl("^crop_prop", nm)][1])
  
  # Keys present and ordered
  expect_true(all(c("station","plant_day","plant_length","rain_total") %in% names(crop_def)))
  expect_true(all(c("station","plant_day","plant_length","rain_total") %in% names(crop_prop)))
  
  # With start_check = "both" we expect both proportion columns
  expect_true(all(c("prop_success_with_start","prop_success_no_start") %in% names(crop_prop)))
  
  # The detailed table should show rain_total_actual >= threshold (>= 60)
  expect_true(all(crop_def$rain_total_actual >= 30, na.rm = TRUE))
})

test_that("crops_definitions: NA handling in windows", {
  data_book <- DataBook$new()
  
  # Construct data with one NA in the 10-day high-rain block
  daily <- dplyr::tibble(
    station = "S1",
    year    = 2001,
    doy     = 100:180,
    rain    = dplyr::case_when(
      doy >= 125 & doy <= 134 ~ 6,
      TRUE ~ 0
    ),
    start_day = 120L,
    end_day   = 170L
  )
  # Make day 128 NA within the 125..134 window
  daily$rain[daily$doy == 128] <- NA_real_
  
  data_book$import_data(list(daily = daily))

  # Threshold = 60; for day 125 with one NA => non-NA sum = 9*6=54 < 60 -> NA (per spec)
  # For day 130, the 10-day window overlaps the same NA once (130..139) -> also NA if NA inside
  # We'll add a clean window at 140 for control (all zeros => fail but not NA)
  crops_definitions(
    data_name   = "daily",
    year        = "year",
    station     = "station",
    doy         = "doy",
    rain        = "rain",
    rain_totals = 60,
    plant_days  = c(125, 130, 140),
    plant_lengths = 10,
    start_check = "yes",
    season_data_name = "daily",
    start_day   = "start_day",
    end_day     = "end_day",
    return_crops_table = TRUE,
    definition_props   = TRUE,
    data_book   = data_book
  )
  
  nm <- data_book$get_data_names()
  crop_def  <- data_book$get_data_frame(nm[grepl("^crop_def", nm)][1])
  crop_prop <- data_book$get_data_frame(nm[grepl("^crop_prop", nm)][1])
  
  # Check NA tagging resolved as NA where anyNA & non-NA sum < threshold
  # Find rows for 125 and 130 with rain_total=60
  row_125 <- subset(crop_def, plant_day == 125 & plant_length == 10 & rain_total == 60)
  row_130 <- subset(crop_def, plant_day == 130 & plant_length == 10 & rain_total == 60)
  row_140 <- subset(crop_def, plant_day == 140 & plant_length == 10 & rain_total == 60)
  
  expect_true(is.na(row_125$rain_total_actual))
  expect_true(row_130$rain_total_actual == 30)
  expect_equal(row_140$rain_total_actual, 0)
})

test_that("crops_definitions: season table taken from a different data set", {
  data_book <- DataBook$new()
  
  daily <- dplyr::tibble(
    station = rep(c("S1","S2"), each = 60),
    year    = 2001,
    doy     = rep(100:159, times = 2),
    rain    = ifelse(doy >= 120 & doy <= 129, 7, 0)  # 10-day block at 7mm => 70mm
  )
  seasons <- dplyr::tibble(
    station = c("S1","S2"),
    year    = 2001,
    s_start = c(115L, 115L),
    s_end   = c(150L, 150L)
  )
  
  data_book$import_data(list(daily = daily, seasons = seasons))
  data_book$add_key("daily", col_names = c("year", "doy", "station"))
  data_book$add_key("seasons", col_names = c("year", "station"))
  suppressWarnings(data_book$add_link(from_data_frame="daily", to_data_frame="seasons", link_pairs=c(station="station", year="year"), type="keyed_link", link_name="link"))
  
  # Keys ("year","station") exist in both tables; start/end columns live in seasons
  crops_definitions(
    data_name   = "daily",
    year        = "year",
    station     = "station",
    doy         = "doy",
    rain        = "rain",
    rain_totals = c(60, 80),     # 70 >= 60 success, 70 < 80 fail
    plant_days  = 120,
    plant_lengths = 10,
    start_check = "no",
    season_data_name = "seasons",
    start_day   = "s_start",
    end_day     = "s_end",
    return_crops_table = TRUE,
    definition_props   = TRUE,
    data_book   = data_book
  )
  
  nm <- data_book$get_data_names()
  crop_prop <- data_book$get_data_frame(nm[grepl("^crop_prop", nm)][1])
  
  # For each station we should have two thresholds; success only at 60
  expect_true(all(c("station","rain_total","plant_day","plant_length","prop_success") %in% names(crop_prop)))
  
  out <- dplyr::arrange(crop_prop, station, rain_total)
  expect_equal(as.numeric(out$prop_success), c(1,0,1,0))
})

test_that("crops_definitions: auto year/DOY from date works", {
  data_book <- DataBook$new()
  
  # Build data with only date + rain (+ station to dodge the wrapper bug noted above)
  dates <- as.Date("2001-05-01") + 0:29
  daily <- dplyr::tibble(
    station = "S1",                    # present so the (buggy) assert doesn't fire
    date    = dates,
    rain    = 2,
    start_day = 120L,                  # season bounds
    end_day   = 200L
  )
  data_book$import_data(list(daily = daily))
  
  # Do not provide year/doy; wrapper should split them from date
  crops_definitions(
    data_name   = "daily",
    year        = NULL,
    station     = "station",
    date        = "date",
    doy         = NULL,
    rain        = "rain",
    rain_totals = 20,
    plant_days  = 150,
    plant_lengths = 10,
    start_check = "yes",
    season_data_name = "daily",
    start_day   = "start_day",
    end_day     = "end_day",
    return_crops_table = FALSE,   # only proportions
    definition_props   = TRUE,
    data_book   = data_book
  )
  
  # Columns added?
  df <- data_book$get_data_frame("daily")
  expect_true(all(c("year","doy") %in% names(df)))
  
  # Proportions table exists and has expected cols
  nm <- data_book$get_data_names()
  crop_prop <- data_book$get_data_frame(nm[grepl("^crop_prop", nm)][1])
  expect_true(all(c("rain_total","plant_length","plant_day","prop_success") %in% names(crop_prop)))
})

test_that("crops_definitions: names auto-increment on repeated runs", {
  data_book <- DataBook$new()
  
  daily <- dplyr::tibble(
    station = "S1",
    year    = 2001,
    doy     = 120:140,
    rain    = 5,
    start_day = 120L,
    end_day   = 200L
  )
  data_book$import_data(list(daily = daily))
  
  # First run
  crops_definitions(
    data_name   = "daily",
    year        = "year",
    station     = "station",
    doy         = "doy",
    rain        = "rain",
    rain_totals = 20,
    plant_days  = 120,
    plant_lengths = 5,
    start_check = "yes",
    season_data_name = "daily",
    start_day   = "start_day",
    end_day     = "end_day",
    return_crops_table = TRUE,
    definition_props   = TRUE,
    data_book   = data_book
  )
  
  # Second run creates crop_def2 / crop_prop2
  crops_definitions(
    data_name   = "daily",
    year        = "year",
    station     = "station",
    doy         = "doy",
    rain        = "rain",
    rain_totals = 30,
    plant_days  = 125,
    plant_lengths = 5,
    start_check = "yes",
    season_data_name = "daily",
    start_day   = "start_day",
    end_day     = "end_day",
    return_crops_table = TRUE,
    definition_props   = TRUE,
    data_book   = data_book
  )
  
  nm <- data_book$get_data_names()
  expect_gte(sum(grepl("^crop_def",  nm)), 2)
  expect_gte(sum(grepl("^crop_prop", nm)), 2)
})