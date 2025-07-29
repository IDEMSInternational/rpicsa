library(databook)
library(dplyr)
library(lubridate)
devtools::load_all()  # loads instatCalculations and your start_rains()

# Helper to run and fetch result
run_start <- function(df, args){
  db <- DataBook$new()
  db$import_data(list(d = df))
  do.call(start_rains, c(list(data = "d"), args, list(data_book = db)))
  # "d_by_station_year" or "d_by_year" depending on station
  out_name <-
    if (!is.null(args$station)) "d_by_station_name_year" else "d_by_year"
  db$get_data_frame(out_name)
}

# 1) Single-day threshold (default) – doy, date, status
test_that("single-day threshold gives correct doy, date, status", {
  df <- tibble::tibble(
    date = as.Date("2000-01-01") + 0:4,
    station_name = "S1",
    rain = c(0, 1, 2, 4, 0)
  )
  res <- suppressWarnings(run_start(df, list(
    date_time = "date",
    station   = "station_name",
    rain      = "rain",
    threshold = 0.85,
    start_day = 2,
    total_rainfall_over_days = 2,
    amount_rain              = 4
  )))
  
  # first day >=0.85 is 3rd row: doy=3, date="2000-01-03", status=TRUE
  expect_equal(as.numeric(res$start_rain), 4)
  expect_equal(as.numeric(res$start_rain_date), as.numeric(as.Date("2000-01-04")))
  expect_true(res$start_rain_status)
})

# 3) proportion definition
test_that("proportion definition", {
  df <- tibble::tibble(
    date = as.Date("2000-01-01") + 0:9,
    station_name="S1",
    rain = 1:10
  )
  # 80th percentile of rolling sum over 3 days:
  # sums = 1+2+3=6,2+3+4=9,...,8+9+10=27 → 80th%ile = 0.8*(27-6)+6 = 0.8*21+6 = 23.8 approx
  res <- suppressWarnings(run_start(df, list(
    date_time                = "date",
    station                  = "station_name",
    rain                     = "rain",
    total_rainfall_comparison = "proportion",
    start_day                = 3,
    total_rainfall_over_days = 3,
    prob_rain_day            = 0.8,
    output                   = "doy"
  )))
  expect_equal(as.numeric(res$start_rain), 9)
})

# 4) evaporation definition
test_that("evaporation definition", {
  df <- tibble::tibble(
    date = as.Date("2000-01-01") + 0:4,
    station_name="S1",
    rain = c(5,5,5,5,5),
    evap = c(1,2,3,4,5)
  )
  res <- suppressWarnings(run_start(df, list(
    date_time                = "date",
    station                  = "station_name",
    rain                     = "rain",
    total_rainfall_comparison = "evaporation",
    total_rainfall_over_days = 2,
    start_day                = 2,
    evaporation_variable     = "evap",
    fraction                 = 0.5,
    output                   = "doy"
  )))
  expect_equal(as.numeric(res$start_rain), 2)
})

# 5) number_rain_days
test_that("number of rainy days definition", {
  df <- tibble::tibble(
    date = as.Date("2000-01-01") + 0:4,
    station_name="S1",
    rain = c(0,1,1,0,0)
  )
  res <- suppressWarnings(run_start(df, list(
    date_time      = "date",
    station        = "station_name",
    rain           = "rain",
    total_rainfall_over_days = 1,
    amount_rain        = 1,
    number_rain_days   = TRUE,
    start_day          = 2, 
    min_rain_days      = 2,
    rain_day_interval  = 3
  )))
  expect_true(is.na(as.numeric(res$start_rain)))
  expect_true(is.na(as.numeric(res$start_rain_date)))
  expect_true(res$start_rain_status)
})

# 6) dry_spell
test_that("dry spell definition", {
  df <- tibble::tibble(
    date = as.Date("2000-01-01") + 0:14,
    station_name="S1",
    rain = c(10,10,0,0,10,0,0,0,0,10,10,10,10,10,10)
  )
  res <- suppressWarnings(run_start(df, list(
    date_time        = "date",
    station          = "station_name",
    rain             = "rain",
    start_day          = 2, 
    total_rainfall_over_days = 1,
    amount_rain        = 1,
    dry_spell        = TRUE,
    spell_interval   = 3,
    spell_max_dry_days = 2,
    output           = "doy"
  )))
  # defines start when a 3-day window has <=2 dry days; first window days1-3 has rain at day1 → dry days=2 -> OK end=3
  expect_equal(as.numeric(res$start_rain), 2)
})

# 7) dry_period
test_that("dry period definition", {
  df <- tibble::tibble(
    date = as.Date("2000-01-01") + 0:15,
    station_name="S1",
    rain = c(10,0,0,0,0,10,0,0,0,0,10,10,10,10,10,10)
  )
  res <- suppressWarnings(run_start(df, list(
    date_time          = "date",
    station            = "station_name",
    rain               = "rain",
    total_rainfall_over_days = 1,
    amount_rain        = 1,
    dry_period         = TRUE,
    period_interval    = 4,
    max_rain           = 1,
    period_max_dry_days = 3,
    output             = "doy"
  )))
  expect_equal(as.numeric(res$start_rain), 11)
})

# 8) include_status = TRUE
test_that("include status works", {
  df <- tibble::tibble(
    date = c(as.Date("2000-01-01") + 0:2),
    station_name="S1",
    rain = c(0,0,0)
  )
  res <- suppressWarnings(run_start(df, list(
    date_time        = "date",
    station          = "station_name",
    rain             = "rain",
    total_rainfall_over_days = 1,
    amount_rain        = 1,
    drop = FALSE,
    output           = c("doy", "status")
  )))
  expect_true("start_rain_status" %in% names(res))
  expect_false(res$start_rain_status)
})

# 9) error on invalid intervals
test_that("invalid interval arguments throw", {
  df <- tibble::tibble(date=Sys.Date()+1:3, rain=1:3)
  expect_error(
    suppressWarnings(start_rains("d", "date", rain="rain",
                                 number_rain_days=TRUE, min_rain_days=3, rain_day_interval=2,
                                 data_book = {db<-DataBook$new(); db$import_data(list(d=df)); db}),
                     "rain_day_interval"
    ))
  expect_error(
    suppressWarnings(start_rains("d", "date", rain="rain",
                                 dry_spell=TRUE, spell_interval=1, spell_max_dry_days=2,
                                 data_book = {db<-DataBook$new(); db$import_data(list(d=df)); db}),
                     "spell_interval"
    ))
  expect_error(
    suppressWarnings(start_rains("d", "date", rain="rain",
                                 dry_period=TRUE, period_interval=2, period_max_dry_days=3,
                                 data_book = {db<-DataBook$new(); db$import_data(list(d=df)); db}),
                     "period_interval"
    ))
})
