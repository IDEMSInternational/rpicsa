#' Get Spells Data
#' 
#' This function calculates rainfall or climatic spells
#' 
#' @param data A data frame containing the data to be analysed.
#' @param summary_data \code{character(1)} Name of the summary data frame in \code{data_book}
#'   that contains the season boundary columns (e.g. \code{start_rain}, \code{end_rains}).
#' @param date_time \code{character(1)} Column name (in \code{data}) of the date variable.
#' @param year \code{character(1)} Column name (in \code{data}) of the year variable.
#'   If \code{NULL}, it is created from \code{date_time} using \code{lubridate::year()} with
#'   \code{s_start_month} applied.
#' @param element The name of the column in 'data' for which extremes are to be found.
#' @param station The name of the `station` column in 'data'. Default `NULL`.
#' @param s_start_month \code{integer(1)} Month (1–12) to treat as the start of the “statistical year”
#'   when deriving \code{year} or \code{doy} from \code{date_time}. Default \code{1} (January).
#' @param doy \code{character(1)} The name of the day of year column in \code{data} to apply the function to. If \code{NULL} it will be created using the \code{date_time} variable.
#' @param day_from \code{character(1)} Column name in \code{summary_data} giving the start of rains.
#' @param day_to \code{character(1)} Column name in \code{summary_data} giving the end of season.
#' @param day_from A numeric value for the start day in the year (numeric, 1–366) to use if `day_from` is `NULL`. 
#' @param day_to A numeric value for the end day in the year (numeric, 1–366) to use if `day_to` is `NULL`. 
#' @param direction A character string specifying the direction for the operation. It can be either `"greater"`, `"less"`, `"between"`, or `"outer"`.
#' @param value A numeric value specifying the threshold value (e.g., 50 mm for rainfall). This is then the upper bound value if `direction == "between"` or `direction == "outer"`.
#' @param lb_value A numeric value for the lower bound if `direction == "between"` or `direction == "outer"`.
#' @param return_max_spell A boolean (default `TRUE`) to return the duration of the longest spell by year (and station)
#' @param return_all_spells A boolean (default `FALSE`) to return a data frame containing all end dates of the defined spell across years (and stations).
#' @param data_book The \code{DataBook} (R6) object holding \code{data} and \code{summary_data}.
#' 
#' @export
#' @return The calculated spells
#' 
#' @examples
#' # Example: Get the length of the longest spell between day 1 and 366 in the data
#' library(databook)
#' data_book <- DataBook$new()
#' daily_data <- rpicsa::daily_niger |>
#'   dplyr::filter(year > 1945, year <= 1950, station_name == "Agades") |>
#'   dplyr::mutate(year = as.numeric(year))
#' data_book$import_data(list(daily_data = daily_data))
#' 
#' get_spells_data(data = "daily_data",
#'                 date_time = "date",
#'                 year = "year",
#'                 station = "station_name",
#'                 element = "rain",
#'                 doy = "doy", 
#'                 day_from = 365,
#'                 day_to = 366,
#'                 direction = "less",
#'                 value = 0.85,
#'                 return_all_spells = TRUE,
#'                 return_max_spell = TRUE,
#'                 data_book = data_book)
#' data_book$get_data_frame("daily_data_by_station_name_year")
#' data_book$get_data_frame("spells_filter")
#' 
#' # Example 2: Compute start/end for daily_niger, then we can use that to get the spells
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
#'   output = c("doy", "status"), data_book = data_book
#' )
#' 
#' end_rains(
#'   data = "daily_data", date_time = "date", station = "station_name",
#'   year = "year", rain = "rain", start_day = 121, end_day = 300,
#'   output = c("doy", "status"), data_book = data_book
#' )
#' 
#' # Get the spells from the start to end of rains
#' get_spells_data(data = "daily_data",
#'                 date_time = "date",
#'                 year = "year",
#'                 station = "station_name",
#'                 element = "rain",
#'                 doy = "doy", 
#'                 summary_data = "daily_data_by_station_name_year",
#'                 day_from = "start_rain",
#'                 day_to = "end_rains",
#'                 direction = "less",
#'                 value = 0.85,
#'                 return_all_spells = TRUE,
#'                 return_max_spell = TRUE,
#'                 data_book = data_book)
#' data_book$get_data_frame("daily_data_by_station_name_year")
#' data_book$get_data_frame("spells_filter")
#' 
get_spells_data <- function(data, date_time = NULL, year = NULL, station = NULL, element, doy = NULL, 
                            summary_data = NULL, day_from = 1, day_to = 366, s_start_month = 1,
                            direction = c("greater", "less", "between", "outer"), value = 0.85,
                            lb_value = 0, return_max_spell = TRUE, return_all_spells = FALSE,
                            data_book = data_book){
  
  if (is.null(data_book)){
    data_book <- DataBook$new()
  }
  
  direction <- match.arg(direction)
  
  # Running checks for data
  checkmate::assert_string(data)
  checkmate::assert_string(element)
  checkmate::assert_string(date_time, null.ok = TRUE)
  checkmate::assert_string(year, null.ok = TRUE)
  checkmate::assert_string(station, null.ok = TRUE)
  checkmate::assert_string(doy, null.ok = TRUE)
  data_frame <- data_book$get_data_frame(data)
  assert_column_names(data_frame, element)
  checkmate::assert(checkmate::check_date(data_frame[[date_time]]), 
                    checkmate::check_posixct(data_frame[[date_time]]))
  if (!is.null(date_time)) assert_column_names(data_frame, date_time)
  if (!is.null(year)) assert_column_names(data_frame, year)
  if (!is.null(station)) assert_column_names(data_frame, station)
  if (!is.null(doy)) assert_column_names(data_frame, doy)
  
  checkmate::assert_string(summary_data, null.ok = TRUE)
  checkmate::assert_int(s_start_month, lower = 1, upper = 12)
  checkmate::assert_numeric(value)
  checkmate::assert_numeric(lb_value)
  checkmate::assert_string(direction)
  
  if (is.numeric(day_from)){
    checkmate::assert_int(day_from, lower = 1, upper = 366)
  } else {
    checkmate::assert_string(day_from, null.ok = TRUE)
  }
  if (is.numeric(day_to)){
    checkmate::assert_int(day_to, lower = 1, upper = 366)
  } else {
    checkmate::assert_string(day_to, null.ok = TRUE)
  }
  
  if (is.null(year)) {
    data_book$split_date(data_name = data, col_name = date_time, year_val = TRUE, s_start_month = s_start_month)
    year <- "year"
  }
  if (is.null(doy)){
    data_book$split_date(data_name = data, col_name = date_time, day_in_year_366 = TRUE, s_start_month = s_start_month)
    doy <- "doy"
  }
  
  fn_exps <- dplyr::case_match(
    direction,
    "greater" ~ paste0("(", element, " >= ", value, ")"),
    "less"    ~ paste0("(", element, " <= ", value, ")"),
    "between" ~ paste0("(", element, " >= ", lb_value, ") & (", element, " <= ", value, ")"),
    "outer"   ~ paste0("(", element, " <= ", lb_value, ") & (", element, " >= ", value, ")")
  )
  
  spell_day <- instatCalculations::instat_calculation$new(
    type = "calculation", 
    function_exp = fn_exps, 
    result_name = "spell_day", 
    calculated_from = setNames(list(element), data))
  
  spell_length <- instatCalculations::instat_calculation$new(
    type = "calculation", 
    # setting initial value as 0 for where we have filtered the doy.
    function_exp = "instatClimatic::spells(x = spell_day, initial_value = 0)",
    result_name = "spell_length", 
    sub_calculations = list(spell_day), 
    save = 0)
  
  # build the grouping vars
  year_grouping <- instatCalculations::instat_calculation$new(
    type = "by", 
    calculated_from = setNames(as.list(year), data))
  
  if (!is.null(station)){
    station_grouping <- instatCalculations::instat_calculation$new(
      type = "by", 
      calculated_from = setNames(as.list(station), data))
  }
  
  if (return_max_spell){
    # Checking if the column names for start and end rains were given, else use the numeric values (1 to 366).
    if (!is.numeric(day_from) & !is.numeric(day_to)){
      calc_from <- setNames(list("doy", c(day_from, day_to)), c(data, summary_data))
    } else {
      calc_from <- setNames(list("doy"), c(data))
    }
    
    fn_day_exps <- paste0(doy, " >= ", day_from, " & ", doy, " <= ", day_to)
    day_from_and_to <- instatCalculations::instat_calculation$new(
      type = "filter", 
      function_exp = fn_day_exps, 
      calculated_from = databook::calc_from_convert(x = calc_from))
    
    # Build manipulations list
    manips <- list(spell_length, year_grouping, day_from_and_to)
    if (exists("station_grouping")) {
      manips <- append(list(station_grouping), manips)
    }
    
    # Now create the calculation
    spells <- instatCalculations::instat_calculation$new(
      type = "summary", 
      function_exp = "max(x = spell_length)", 
      result_name = "spells", 
      manipulations = manips, 
      save = 2
    )
    
    data_book$run_instat_calculation(calc = spells, display = FALSE)
  }
  
  if (return_all_spells){
    # if we return all spells, we do the DOY filter last. Meaning, we are filtering on our new df for DOY
    # so we need data to instead be the name of the new data frame: spells_filter
    # but then what if it's called spells_filter1, etc?
    # so we do not yet offer filtering the day. The user has to do that afterwards. 
    
    spells_filter <- instatCalculations::instat_calculation$new(
      type = "filter", 
      function_exp = "dplyr::lead(c(NA, diff(spell_length))) < 0", 
      sub_calculations = list(spell_length), 
      save = 2, 
      result_data_frame = "spells_filter")
    
    manips <- list(spells_filter, year_grouping)
    if (exists("station_grouping")) {
      manips <- append(list(station_grouping), manips)
    }
    
    spells_filter_combined <- instatCalculations::instat_calculation$new(
      type = "combination",
      manipulations = manips)
    data_book$run_instat_calculation(calc=spells_filter_combined, display=FALSE)
  }
}