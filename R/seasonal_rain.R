#' Seasonal total rainfall
#' @description Total annual rainfall between start of the rains and end of the season.
#' 
# @inheritParams annual_rain
#' 
#' @param summary_data Summary data frame containing the `start_date` and `end_date` variables. These variables are calculated from start of rains and end of season functions.
#' @param start_date \code{character(1)} The name of the start of rains column in \code{summary_data}.
#' @param end_date \code{character(1)} The name of the end of season column in \code{summary_data}.
#' @param data The daily data frame to calculate rainfall from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param rain \code{character(1)} The name of the rainfall column in \code{data} to apply the function to.
#' @param doy \code{character(1)} The name of the day of year column in \code{data} to apply the function to. If \code{NULL} it will be created using the \code{date_time} variable.
#' @param s_start_month \code{integer(1)} Month (1–12) to treat as the start of the “year” when creating \code{year} or \code{doy}. Default \code{NULL} (assumes January).
#' @param total_rain \code{logical(1)} default `TRUE`. Display the total rainfall value for each year.
#' @param n_rain \code{logical(1)} default `TRUE`. Display the number of rainfall days.
#' @param rain_day \code{numerical(1)} If `n_rain = TRUE`, the minimum rainfall value in a day for that day to count as a rainfall day.
#' @param na_rm \code{logical(1)}. Should missing values (including \code{NaN}) be removed?
#' @param na_prop \code{integer(1)} Max proportion of missing values allowed
#' @param na_n \code{integer(1)} Max number of missing values allowed
#' @param na_consec \code{integer(1)} Max number of consecutive missing values allowed
#' @param na_n_non \code{integer(1)} Min number of non-missing values required
#' @param data_book The data book object where the data object is stored, default `NULL`.
#' 
#' @return A data.frame with rainfall summaries for each year in the specified season (between start of the rains and end of season).
#' @export
#'
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @examples 
#' # Example: First calculate start and end rains
#' data_book <- DataBook$new()
#' daily_data <- rpicsa::daily_niger %>%
#'   dplyr::filter(year <= 1950) %>%
#'   dplyr::filter(year > 1945) %>%
#'   dplyr::mutate(year = as.numeric(year)) %>%
#'   dplyr::filter(station_name == "Agades")
#' data_book$import_data(list(daily_data = daily_data))
#' 
#' start_rains(data = "daily_data",
#'             date_time = "date",
#'             station = "station_name",
#'             year = "year",
#'             rain = "rain",
#'             start_day = 121,
#'             end_day = 300,
#'             output = "doy",
#'             data_book = data_book)
#' 
#' end_rains(data = "daily_data",
#'           date_time = "date",
#'           station = "station_name",
#'           year = "year",
#'           rain = "rain",
#'           start_day = 121,
#'           end_day = 300,
#'           output = "doy",
#'           data_book = data_book)
#' 
#' # then run the summaries for seasonal rainfall
#' seasonal_rain(summary_data = "daily_data_by_station_name_year",
#'               start_date = "start_rain",
#'               end_date = "end_rains",
#'               data = "daily_data",
#'               date_time = "date",
#'               year = "year",
#'               doy = "doy",
#'               station = "station_name",
#'               rain = "rain",
#'               data_book = data_book
#' )
#' daily_data_by_station_name_year <- data_book$get_data_frame("daily_data_by_station_name_year")
#' daily_data_by_station_name_year
seasonal_rain <- function (summary_data = NULL, start_date = NULL, end_date = NULL,
                           data, date_time, year = NULL, station = NULL, doy = NULL, 
                           rain = NULL,  s_start_month = 1, total_rain = TRUE, 
                           n_rain = TRUE, rain_day = 0.85, na_rm = FALSE,
                           na_prop = NULL, na_n = NULL, na_consec = NULL, 
                           na_n_non = NULL,  data_book = NULL) {
  if (is.null(data_book)){
    data_book = DataBook$new()
  }

  # Running checks
  # checks with the summary data frame
  checkmate::assert_character(summary_data)
  checkmate::assert_character(start_date)
  checkmate::assert_character(end_date)
  data_frame <- data_book$get_data_frame(summary_data)
  assert_column_names(data_frame, start_date)
  assert_column_names(data_frame, end_date)
  
  # checks with the data frame
  checkmate::assert_character(data)
  checkmate::assert_character(date_time, null.ok = TRUE)
  checkmate::assert_character(rain)
  checkmate::assert_string(station, null.ok = TRUE)
  checkmate::assert_string(year, null.ok = TRUE)
  checkmate::assert_string(doy, null.ok = TRUE)
  data_frame <- data_book$get_data_frame(data)
  assert_column_names(data_frame, rain)
  checkmate::assert(checkmate::check_date(data_frame[[date_time]],), 
                    checkmate::check_posixct(data_frame[[date_time]],  null.ok = TRUE))
  if (!is.null(station)) assert_column_names(data_frame, station)
  if (!is.null(date_time)) assert_column_names(data_frame, date_time)
  if (!is.null(year)) assert_column_names(data_frame, year)  
  if (!is.null(doy)) assert_column_names(data_frame, doy)  
  
  # checks for summaries
  checkmate::assert_int(s_start_month, lower = 1, upper = 12)
  checkmate::assert_numeric(rain_day)
  checkmate::assert_logical(total_rain, null.ok = TRUE)
  checkmate::assert_logical(n_rain, null.ok = TRUE)
  checkmate::assert_logical(na_rm, null.ok = TRUE)
  checkmate::assert_int(na_prop, null.ok = TRUE)
  checkmate::assert_int(na_n, null.ok = TRUE)
  checkmate::assert_int(na_consec, null.ok = TRUE)
  checkmate::assert_int(na_n_non, null.ok = TRUE)
  
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
  
  na_type <- c(
    if (!is.null(na_n))        "n",
    if (!is.null(na_n_non))    "n_non_miss",
    if (!is.null(na_prop))     "prop",
    if (!is.null(na_consec))   "con"
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