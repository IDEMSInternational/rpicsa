#' Get Extreme Data
#' 
#' This function identifies extreme values in a specified element (column) of a data frame. It can operate in two modes: percentile-based and threshold-based.
#' 
#' @param data A data frame containing the data to be analysed.
#' @param date_time \code{character(1)} Column name (in \code{data}) of the date variable.
#' @param year \code{character(1)} Column name (in \code{data}) of the year variable.
#'   If \code{NULL}, it is created from \code{date_time} using \code{lubridate::year()} with
#'   \code{s_start_month} applied.
#' @param element The name of the column in 'data' for which extremes are to be found.
#' @param station The name of the `station` column in 'data'. Default `NULL`.
#' @param s_start_month \code{integer(1)} Month (1–12) to treat as the start of the “statistical year”
#'   when deriving \code{year} or \code{doy} from \code{date_time}. Default \code{1} (January).
#' @param value A numeric value specifying the threshold value (e.g., 50 mm for rainfall). This is then the upper bound value if `direction == "between"` or `direction == "outer"`.
#' @param direction A character string specifying the direction for the operation. It can be either `"greater"`, `"less"`, `"between"`, or `"outer"`.
#' @param lb_value A numeric value for the lower bound if `direction == "between"` or `direction == "outer"`.
#' @param na_rm \code{logical(1)} Should missing values be removed before summing? Passed through to
#'   the summary calculation. Default \code{FALSE}.
#' @param na_prop \code{numeric(1)} Maximum allowed proportion of missing values in the in-season window.
#' @param na_n \code{integer(1)} Maximum allowed number of missing values.
#' @param na_consec \code{integer(1)} Maximum allowed number of consecutive missing values.
#' @param na_n_non \code{integer(1)} Minimum required count of non-missing values.
#' @param data_book The \code{DataBook} (R6) object holding \code{data} and \code{summary_data}.
#' 
#' @export
#' @return A filtered data frame where the `element` values are considered extreme based on the specified `value` and `direction`.
#' 
#' @examples 
#' library(databook)
#' data_book <- DataBook$new()
#' daily_data <- rpicsa::daily_niger |>
#'   dplyr::filter(year > 1945, year <= 1955, station_name == "Agades") |>
#'   dplyr::mutate(year = as.numeric(year))
#' data_book$import_data(list(daily_data = daily_data))
#' 
#' # Looking at the number of instances in a year that rainfall exceeds 40mm in a day
#' get_extremes(data = "daily_data",
#'              date_time  = "date",
#'              station = "station_name",
#'              element = "rain",
#'              direction = "greater",
#'              value = 40,
#'              data_book = data_book)
#'              
#' # The extreme days are under "sum_extreme_rain" here:
#' data_book$get_data_frame("daily_data_by_station_name_year")
get_extremes <- function(data, element, date_time = NULL, year = NULL, station = NULL, 
                         direction = c("greater", "less", "between", "outer"), 
                         s_start_month = 1, value = 0.85, lb_value = 0, na_rm = FALSE,
                         na_prop = NULL, na_n = NULL, na_consec = NULL, na_n_non = NULL,  
                         data_book = data_book) {
  
  if (is.null(data_book)){
    data_book = DataBook$new()
  }
  
  direction <- match.arg(direction)
  
  # Running checks for data
  checkmate::assert_string(data)
  checkmate::assert_string(element)
  checkmate::assert_string(date_time, null.ok=TRUE)
  checkmate::assert_string(year, null.ok = TRUE)
  checkmate::assert_string(station, null.ok = TRUE)
  data_frame <- data_book$get_data_frame(data)
  assert_column_names(data_frame, element)
  checkmate::assert(checkmate::check_date(data_frame[[date_time]]), 
                    checkmate::check_posixct(data_frame[[date_time]]))
  if (!is.null(date_time)) assert_column_names(data_frame, date_time)
  if (!is.null(year)) assert_column_names(data_frame, year)
  if (!is.null(station)) assert_column_names(data_frame, station)
  
  checkmate::assert_int(s_start_month, lower = 1, upper = 12)
  checkmate::assert_numeric(value, lower = 0)
  checkmate::assert_numeric(lb_value, lower = 0)
  checkmate::assert_string(direction)
  checkmate::assert_logical(na_rm, null.ok = TRUE)
  checkmate::assert_numeric(na_prop, lower = 0, null.ok = TRUE)
  checkmate::assert_numeric(na_n, lower = 0, null.ok = TRUE)
  checkmate::assert_numeric(na_consec, lower = 0, null.ok = TRUE)
  checkmate::assert_numeric(na_n_non, lower = 0, null.ok = TRUE)
  
  if (is.null(year)) {
    data_book$split_date(data_name = data, col_name = date_time, year_val = TRUE, s_start_month = s_start_month)
    year <- "year"
  }
  
  fn_exps <- dplyr::case_match(
    direction,
    "greater" ~ paste0("(", element, " >= ", value, ")"),
    "less"    ~ paste0("(", element, " <= ", value, ")"),
    "between" ~ paste0("(", element, " >= ", lb_value, ") & (", element, " <= ", value, ")"),
    "outer"   ~ paste0("(", element, " <= ", lb_value, ") & (", element, " >= ", value, ")")
  )
  
  rain_day <- instatCalculations::instat_calculation$new(
    type = "calculation", 
    function_exp = fn_exps, 
    result_name = "rain_day", 
    calculated_from = setNames(list(element), data))
  
  # Then we give a 0/1 depending if it is in that bracket or not
  transform_calculation <- instatCalculations::instat_calculation$new(
    type = "calculation", 
    function_exp = "zoo::rollapply(data = rain_day, width = 1, FUN = sum, align = 'right', fill = NA)", 
    result_name = paste0("extreme_", element), 
    sub_calculations = list(rain_day), 
    save = 2, 
    before = FALSE, 
    adjacent_column = element)
  data_book$run_instat_calculation(calc = transform_calculation, display = FALSE)
  
  summary_calculation(data = data,
                      date_time = date_time,
                      station = station,
                      year = year,
                      to = "annual",
                      columns_to_summarise = paste0("extreme_", element), 
                      summaries = "sum",
                      na_rm = na_rm,
                      na_prop = na_prop,
                      na_n = na_n,
                      na_consec = na_consec,
                      na_n_non = na_n_non,
                      data_book = data_book)
  
}
