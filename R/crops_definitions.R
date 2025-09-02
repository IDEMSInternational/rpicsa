#' Crop Definitions
#' @description
#' Computes, for many candidate planting windows, whether a rainfall threshold
#' was met within the window and within seasonal bounds, then imports two tables
#' into the DataBook:
#' - **`crop_def*`** (row-level windows with conditions and actual rainfall)
#' - **`crop_prop*`** (aggregated proportions of successful windows)
#' Names are auto-incremented if they already exist (e.g., `crop_def2`).
#'
#' @param data_name `character(1)` Name of the daily data frame.
#' @param year `character(1)` Column name in `data_name` giving the year.
#' @param date `character(1)`  optional. Column name in `data_name` giving the date .
#' @param station `character(1)` Optional. Column name giving the station ID.
#'   If missing, results are computed by year only.
#' @param rain `character(1)` Column name giving daily rainfall (numeric).
#' @param doy `character(1)` Column name giving the doy.
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
#' @examples #TODO
crops_definitions <- function (data_name, year, station, rain, doy, rain_totals, 
                               plant_days, plant_lengths, start_check = c("both", "yes","no"), 
                               season_data_name, start_day, end_day, return_crops_table = TRUE, 
                               definition_props = TRUE, date=NULL, data_book = NULL){
    if (is.null(data_book)){
      data_book = DataBook$new()
    }
  
    
  
    # Running checks
    checkmate::assert_string(data_name)
    checkmate::assert_string(rain)
    checkmate::assert_string(year, null.ok = TRUE)
    checkmate::assert_string(date, null.ok = TRUE)
    checkmate::assert_string(station)
    checkmate::assert_string(doy, null.ok = TRUE)
    checkmate::assert_numeric(rain_totals)
    checkmate::assert_numeric(plant_days)
    checkmate::assert_numeric(plant_lengths)
  
    checkmate::assert_string(season_data_name)
    checkmate::assert_string(start_day)
    checkmate::assert_string(end_day)
  
    data_frame <- data_book$get_data_frame(data_name)
    if (!is.null(date)) assert_column_names(data_frame, date)
    if (!is.null(year)) assert_column_names(data_frame, year)
    if (!is.null(doy)) assert_column_names(data_frame, doy)
    assert_column_names(data_frame, rain)
    assert_column_names(data_frame, station)
    assert_column_names(data_frame, rain_totals)
    assert_column_names(data_frame, plant_days)
    assert_column_names(data_frame, plant_lengths)

    
    data_frame <- data_book$get_data_frame(season_data_name)
    assert_column_names(data_frame, start_day)
    assert_column_names(data_frame, end_day)

    checkmate::assert_logical(return_crops_table)
    checkmate::assert_logical(definition_props)

    assert_column_names(data_frame, start_day)
    assert_column_names(data_frame, end_day)
    
    checkmate::assert_logical(return_crops_table)
    checkmate::assert_logical(definition_props)
  
    # calculate doy, year from date
    if (is.null(year)) {
        data_book$split_date(data_name = data_name, col_name = date, year_val = TRUE, s_start_month = 1)
        year <- "year"
    }
    
    if (is.null(doy)){
        data_book$split_date(data_name = data_name, col_name = date, day_in_year_366 =TRUE, s_start_month = 1)
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
        definition_props  = definition_props))
         
}
