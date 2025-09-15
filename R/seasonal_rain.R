#' Seasonal total rainfall
#'
#' Compute the seasonal total rainfall between the start of the rains and the end
#' of the season. The function filters daily data to the interval
#' `start_date`, `end_date` (both inclusive) for each grouping (year and, if
#' provided, station) and then summarises the `rain` column. Results are written
#' back into the `summary_data` table stored in `data_book`.
#'
#' @param summary_data \code{character(1)} Name of the summary data frame in \code{data_book}
#'   that contains the season boundary columns (e.g. \code{start_rain}, \code{end_rains}).
#' @param start_date \code{character(1)} Column name in \code{summary_data} giving the start of rains
#'   (typically a day-of-year integer).
#' @param end_date \code{character(1)} Column name in \code{summary_data} giving the end of season
#'   (typically a day-of-year integer).
#' @param s_start_month \code{integer(1)} Month (1–12) to treat as the start of the “statistical year”
#'   when deriving \code{year} or \code{doy} from \code{date_time}. Default \code{1} (January).
#' @param data \code{character(1)} Name of the daily data frame in \code{data_book}.
#' @param date_time \code{character(1)} Column name (in \code{data}) of the date variable.
#' @param year \code{character(1)} Column name (in \code{data}) of the year variable.
#'   If \code{NULL}, it is created from \code{date_time} using \code{lubridate::year()} with
#'   \code{s_start_month} applied.
#' @param station \code{character(1)} Optional station column in \code{data} (useful when \code{data}
#'   contains multiple stations). If supplied, summaries are grouped by \code{station} and \code{year}.
#' @param doy \code{character(1)} Column name (in \code{data}) of the day-of-year variable.
#'   If \code{NULL}, it is created from \code{date_time} (366-day DOY) with \code{s_start_month} applied.
#' @param rain \code{character(1)} Column name (in \code{data}) of the rainfall variable to summarise.
#' @param total_rain \code{logical(1)} Compute and store the seasonal total rainfall. Default \code{TRUE}.
#' @param n_rain \code{logical(1)} (Reserved) Compute the number of rainfall days in-season.
#'   Currently not implemented. Default \code{TRUE} (ignored).
#' @param rain_day \code{numeric(1)} (Reserved) Threshold (e.g., \code{0.85}) above which a day
#'   counts as a rainfall day when \code{n_rain = TRUE}. Currently ignored.
#' @param na_rm \code{logical(1)} Should missing values be removed before summing? Passed through to
#'   the summary calculation. Default \code{FALSE}.
#' @param na_prop \code{numeric(1)} Maximum allowed proportion of missing values in the in-season window.
#' @param na_n \code{integer(1)} Maximum allowed number of missing values.
#' @param na_consec \code{integer(1)} Maximum allowed number of consecutive missing values.
#' @param na_n_non \code{integer(1)} Minimum required count of non-missing values.
#' @param data_book The \code{DataBook} (R6) object holding \code{data} and \code{summary_data}.
#'   If \code{NULL}, a new one is created internally (side effects then apply to that object).
#'
#' @details
#' If \code{doy} and/or \code{year} are not provided, they are created from \code{date_time}
#' using \code{data_book$split_date()}, with \code{s_start_month} defining the start of the statistical year.
#' The function constructs a row-wise filter selecting days \code{doy >= start_date & doy <= end_date}
#' and then calls \code{data_book$calculate_summary()} with \code{summaries = "summary_sum"} on \code{rain},
#' grouped by \code{year} (and \code{station}, if supplied).
#'
#' The newly computed summary column is appended to \code{summary_data} in \code{data_book}. The exact
#' name of the column depends on the \code{DataBook} summary-naming scheme (commonly includes “sum”
#' and the variable name, e.g. \code{sum_rain}).
#'
#' If both \code{total_rain} and \code{n_rain} are \code{FALSE}, the function stops with an error.
#' Note: as of now, only \code{total_rain} is implemented; \code{n_rain} / \code{rain_day} are reserved.
#'
#' @export
#' @return
#' Invisibly returns \code{NULL}. The primary effect is to modify \code{summary_data} inside
#' \code{data_book} by adding a seasonal rainfall total column (and, in future, a rainfall-day count).
#' Retrieve results with \code{data_book$get_data_frame(summary_data)}.
#'
#' @seealso \code{\link{start_rains}}, \code{\link{end_rains}}, \code{\link{seasonal_length}}
#'
#' @examples
#' # Example workflow: compute start/end, then seasonal totals for Agades (1946–1950 subset)
#' library(databook)
#' data_book <- DataBook$new()
#' daily_data <- rpicsa::daily_niger |>
#'   dplyr::filter(station_name == "Agades", year > 1945, year <= 1950) |>
#'   dplyr::mutate(year = as.numeric(year))
#' data_book$import_data(list(daily_data = daily_data))
#'
#' start_rains(
#'   data = "daily_data", date_time = "date", station = "station_name",
#'   year = "year", rain = "rain", start_day = 121, end_day = 300,
#'   output = "doy", data_book = data_book
#' )
#'
#' end_rains(
#'   data = "daily_data", date_time = "date", station = "station_name",
#'   year = "year", rain = "rain", start_day = 121, end_day = 300,
#'   output = "doy", data_book = data_book
#' )
#'
#' seasonal_rain(
#'   summary_data = "daily_data_by_station_name_year",
#'   start_date   = "start_rain",
#'   end_date     = "end_rains",
#'   data         = "daily_data",
#'   date_time    = "date",
#'   year         = "year",
#'   doy          = "doy",
#'   station      = "station_name",
#'   rain         = "rain",
#'   data_book    = data_book
#' )
#'
#' # Inspect results
#' data_book$get_data_frame("daily_data_by_station_name_year")
seasonal_rain <- function (summary_data = NULL, start_date = NULL, end_date = NULL,
                           data, date_time, year = NULL, station = NULL, doy = NULL, 
                           rain = NULL,  s_start_month = 1, total_rain = TRUE, 
                           n_rain = TRUE, rain_day = 0.85, na_rm = FALSE,
                           na_prop = NULL, na_n = NULL, na_consec = NULL, 
                           na_n_non = NULL,  data_book = data_book) {
  if (is.null(data_book)){
    data_book = DataBook$new()
  }

  # Running checks
  # checks with the summary data frame
  checkmate::assert_string(summary_data, null.ok = TRUE)
  checkmate::assert_string(start_date, null.ok = TRUE)
  checkmate::assert_string(end_date, null.ok = TRUE)
  if (!is.null(summary_data)){
      data_frame <- data_book$get_data_frame(summary_data)
      assert_column_names(data_frame, start_date)
      assert_column_names(data_frame, end_date) 
  }
  
  # checks with the data frame
  checkmate::assert_string(data)
  checkmate::assert_string(date_time)
  checkmate::assert_string(rain)
  checkmate::assert_string(station, null.ok = TRUE)
  checkmate::assert_string(year, null.ok = TRUE)
  checkmate::assert_string(doy, null.ok = TRUE)
  data_frame <- data_book$get_data_frame(data)
  assert_column_names(data_frame, rain)
  assert_column_names(data_frame, date_time)
  checkmate::assert(checkmate::check_date(data_frame[[date_time]],), 
                    checkmate::check_posixct(data_frame[[date_time]],  null.ok = TRUE))
  if (!is.null(station)) assert_column_names(data_frame, station)
  if (!is.null(year)) assert_column_names(data_frame, year)  
  if (!is.null(doy)) assert_column_names(data_frame, doy)  
  
  # checks for summaries
  checkmate::assert_int(s_start_month, lower = 1, upper = 12)
  checkmate::assert_numeric(rain_day, lower = 0)
  checkmate::assert_logical(total_rain)
  checkmate::assert_logical(n_rain)
  checkmate::assert_logical(na_rm)
  checkmate::assert_numeric(na_prop, lower = 0, null.ok = TRUE)
  checkmate::assert_numeric(na_n, lower = 0, null.ok = TRUE)
  checkmate::assert_numeric(na_consec, lower = 0, null.ok = TRUE)
  checkmate::assert_numeric(na_n_non, lower = 0, null.ok = TRUE)
  
  # check at least one summary is given  
  if (!total_rain && !n_rain) {
    stop("No summaries selected. At least one of\n         'total_rain' or 'n_rain' must be TRUE.")
  }
  
  # calculate doy, year from date
  if(is.null(doy)){ 
    data_book$split_date(data_name = data,
                         col_name = date_time,
                         day_in_year_366 =TRUE,
                         s_start_month = s_start_month)
    doy <- "doy"
  }
  if (is.null(year)) {
    data_book$split_date(data_name = data, 
                         col_name = date_time, 
                         year_val = TRUE, 
                         s_start_month = s_start_month)
    year <- "year"
  }
  
  # day filter which gets the days from the start of rains to the end of rains
  day_filter <- instatCalculations::instat_calculation$new(
    type="filter", 
    function_exp=paste0(doy, " >= ", start_date, " & ", doy, " <= ", end_date), 
    calculated_from=databook::calc_from_convert(x=setNames(
      list(doy, c(start_date, end_date)),
      c(data, summary_data))))
  
  factors_by <- c(year, station)
  factors_by <- factors_by[!sapply(factors_by, is.null)]
  
  # Defining NA types
  na_type <- c(
    if (!is.null(na_n))        "'n'",
    if (!is.null(na_n_non))    "'n_non_miss'",
    if (!is.null(na_prop))     "'prop'",
    if (!is.null(na_consec))   "'con'"
  )
  
  # then we just calculate the summaries for those days
  data_book$calculate_summary(
    columns_to_summarise = rain, 
    data_name = data, 
    factors = factors_by, 
    additional_filter =day_filter, 
    summaries=c("summary_sum"), 
    silent=TRUE,
    na.rm = na_rm,
    na_type = na_type, 
    na_max_n = na_n,
    na_min_n = na_n_non,
    na_consecutive_n = na_consec,
    na_max_prop = na_prop)
}