#' Crop Definitions
#' @description
#' Computes, for many candidate planting windows, whether a rainfall threshold
#' was met within the window and within seasonal bounds, then imports two tables
#' into the DataBook:
#' - `crop_def*` (row-level windows with conditions and actual rainfall)
#' - `crop_prop*` (aggregated proportions of successful windows)
#' Names are auto-incremented if they already exist (e.g., `crop_def2`).
#'
#' @param data_name `character(1)` Name of the daily data frame.
#' @param date_time `character(1)`  optional. Column name in `data_name` giving the date .
#' @param year \code{character(1)} Column name (in \code{data}) of the year variable.
#'   If \code{NULL}, it is created from \code{date_time} using \code{lubridate::year()} with
#'   \code{s_start_month} applied.
#' @param station `character(1)` Optional. Column name giving the station ID.
#'   If missing, results are computed by year only.
#' @param rain `character(1)` Column name giving daily rainfall (numeric).
#' @param doy \code{character(1)} Column name (in \code{data}) of the day-of-year variable.
#'   If \code{NULL}, it is created from \code{date_time} (366-day DOY) with \code{s_start_month} applied.
#' @param s_start_month \code{integer(1)} Month (1–12) to treat as the start of the “statistical year”
#'   when deriving \code{year} or \code{doy} from \code{date_time}. Default \code{1} (January).
#' @param rain_totals The amount of water (rainfall) needed for the crop, usually between 250mm and 1000mm
#' Enter three comma-separated numbers to generate a sequence: from, to, by; for example, 200, 1200, 50 produces 200, 250, 300, ..., 1200.
#' @param plant_days The day number for planting. Starting from January, April 1st is day 92. Starting from July, November 1st is day 124.
#' Enter three comma-separated numbers to generate a sequence: from, to, by; for example, 93, 183, 15 produces 93, 108, 123, …, 183.
#' @param plant_lengths The crop duration in days. Often between 60 (2 months) and 150 (5 months).
#' Enter three comma-separated numbers to generate a sequence: from, to, by; for example, 45, 180, 30 produces 45, 75, 105, ..., 180.
#' @param start_check `c("both","yes","no")` Whether to (a) compute both start-checked
#'   and no-start variants, (b) require the window to start within the season,
#'   or (c) ignore the seasonal start check (end must still be within season).
#' @param season_data_name `character(1)` Name of the table containing
#'   `start_day`/`end_day`. Defaults to `data_name`.
#' @param start_day `character(1)` Column name of the season start day in the
#'   season data.
#' @param end_day `character(1)` Column name of the season end day in the
#'   season data.
#' @param return_crops_table `logical(1)` If `TRUE`, imports the detailed
#'   row-level window table (`crop_def*`).
#' @param definition_props `logical(1)` If `TRUE`, imports the aggregated
#'   proportions table (`crop_prop*`).
#' @param data_book A \code{DataBook} object to use. If \code{NULL}, a new one is created. Default \code{NULL}.
#'
#' @details
#' For every combination of `plant_day` (start-of-window), `plant_length`
#' (window length in days), and `rain_total` (required cumulative rainfall),
#' the function:
#'
#' 1. Slices the daily series for the given year (and optionally station)
#'    over `[plant_day, plant_day + plant_length)`.
#' 2. Sums rainfall in that slice to `rain_total_actual`.
#' 3. Flags whether the window lies within the seasonal bounds:
#'    `start_day` <= `plant_day` and `plant_day + plant_length` <= `end_day`.
#' 4. Checks if `rain_total_actual` >= `rain_total`.
#' 5. Depending on `start_check`:
#'    - `"yes"`: requires start and end to be within season and threshold met.
#'    - `"no"`: ignores seasonal start; requires end within season and threshold met.
#'    - `"both"`: computes both variants and reports two proportions.
#'
#' NA handling.
#' - If the window contains any NA and the (non-NA) sum would be \< `rain_total`,
#'   the result for that row is set to `NA` (i.e., cannot confirm success).
#' - If the window is shorter than `plant_length` when counting available days,
#'   or all values are NA, the row is `NA`.
#'
#' Side effects / links.
#' - Imports `crop_def*` and/or `crop_prop*`
#' - Creates links between the new tables and (a) the season table (if different)
#'   and (b) between `crop_def*` and `crop_prop*` on the key grid
#'   (`rain_total`, `plant_length`, `plant_day`, and optionally `station`).
#' @return
#' Invisibly returns `NULL`. The primary result is the imported tables and
#' links created inside the DataBook.
#'
#' @export
#'
#' @examples 
#' # Example: compute start/end (DOY) then crop probabilities for Agades (1946–1950 subset)
#' library(databook)
#' data_book <- DataBook$new()
#' daily_data <- rpicsa::daily_niger |>
#'   dplyr::filter(year > 1945, year <= 1950, station_name == "Agades") |>
#'   dplyr::mutate(year = as.numeric(year))
#' data_book$import_data(list(daily_data = daily_data))
#' 
#' start_rains(
#'   data = "daily_data", date_time = "date", station = "station_name",
#'   year = "year", rain = "rain", start_day = 121, end_day = 300,
#'   total_rainfall_over_days = 3,
#'   output = "doy", data_book = data_book
#' )
#' 
#' end_rains(
#'   data = "daily_data", date_time = "date", station = "station_name",
#'   year = "year", rain = "rain", start_day = 121, end_day = 300,
#'   output = "doy", data_book = data_book
#' )
#' 
#' crops_definitions(data_name = "daily_data",
#'                   year = "year",
#'                   station = "station_name",
#'                   doy = "doy",
#'                   rain = "rain",
#'                   rain_totals = c(0, 50),
#'                   plant_days = seq(from = 200, to = 230, by = 15),
#'                   plant_lengths = c(15, 30, 45),
#'                   start_check = "both",
#'                   season_data_name = "daily_data_by_station_name_year",
#'                   start_day = "start_rain",
#'                   end_day = "end_rains",
#'                   return_crops_table = TRUE, 
#'                   definition_props = TRUE,
#'                   data_book = data_book)
#'
#' # View the crop definitions data 
#' head(data_book$get_data_frame("crop_def"))
#' 
#' # View the crop probability summaries
#' data_book$get_data_frame("crop_prop")

crops_definitions <- function(data_name, date_time = NULL, year = NULL, station = NULL,
                              doy = NULL, rain, s_start_month = 1, rain_totals, plant_days, plant_lengths,
                              start_check = c("both", "yes","no"), 
                              season_data_name = NULL, start_day, end_day,
                              return_crops_table = TRUE, 
                              definition_props = TRUE, data_book = data_book){
  if (is.null(data_book)){
    data_book = DataBook$new()
  }
  
  # Running checks for data
  checkmate::assert_string(data_name)
  checkmate::assert_string(rain)
  checkmate::assert_string(year, null.ok = TRUE)
  checkmate::assert_string(date_time, null.ok = TRUE)
  checkmate::assert_string(station, null.ok = TRUE)
  checkmate::assert_string(doy, null.ok = TRUE)
  checkmate::assert_numeric(rain_totals)
  checkmate::assert_numeric(plant_days)
  checkmate::assert_numeric(plant_lengths)
  
  data_frame <- data_book$get_data_frame(data_name)
  if (!is.null(date_time)) assert_column_names(data_frame, date_time)
  if (!is.null(year)) assert_column_names(data_frame, year)
  if (!is.null(doy)) assert_column_names(data_frame, doy)
  if (!is.null(station)) assert_column_names(data_frame, station)
  assert_column_names(data_frame, rain)

  # Running checks for other data
  checkmate::assert_int(s_start_month, lower = 1, upper = 12)
  checkmate::assert_string(season_data_name, null.ok = TRUE)
  checkmate::assert_string(start_day)
  checkmate::assert_string(end_day)
  if (!is.null(season_data_name)){
    data_frame <- data_book$get_data_frame(season_data_name)
    assert_column_names(data_frame, start_day)
    assert_column_names(data_frame, end_day)      
  }
  checkmate::assert_logical(return_crops_table)
  checkmate::assert_logical(definition_props)
  
  # calculate doy, year from date_time
  if (is.null(year)) {
    data_book$split_date(data_name = data_name, col_name = date_time, year_val = TRUE, s_start_month = s_start_month)
    year <- "year"
  }
  
  if (is.null(doy)){
    data_book$split_date(data_name = data_name, col_name = date_time, day_in_year_366 =TRUE, s_start_month = s_start_month)
    doy <- "doy"
  }
  
  return(data_book$crops_definitions(
    data_name = data_name, 
    year = year, 
    station = station, 
    rain = rain, 
    day = doy, 
    rain_totals = rain_totals, 
    plant_days = plant_days, 
    plant_lengths = plant_lengths, 
    start_check  = start_check, 
    season_data_name = season_data_name, 
    start_day = start_day, end_day = end_day, 
    return_crops_table  = return_crops_table, 
    definition_props  = definition_props)
  )
}
