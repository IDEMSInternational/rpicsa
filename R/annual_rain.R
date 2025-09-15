#' Annual total rainfall
#' @description Returns a summary data frame giving the total rainfall each year from 1 Jan to 31 Dec.
#' 
#' @param data The data.frame to calculate from.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param rain \code{character(1)} The name of the rainfall column in \code{data} to apply the function to.
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
#' @return A data.frame with rainfall summaries for each year
#' @export
#'
#' @examples #daily_niger_1 <- daily_niger %>% dplyr::filter(year > 1960)
#' #annual_rain(data = daily_niger, date_time  = "date", station = "station_name",
#' #            rain = "rain", na_prop = 0.9)
annual_rain <- function(data, year = NULL, station = NULL, rain,
                        total_rain = TRUE, n_rain = TRUE, rain_day = 0.85,
                        na_rm = FALSE, na_prop = NULL, na_n = NULL, na_consec = NULL,
                        na_n_non = NULL, data_book = NULL) {
  if (is.null(data_book)) {
    data_book <- DataBook$new()
  }
  
  # running checks
  checkmate::assert_string(data)
  checkmate::assert_string(rain)
  checkmate::assert_string(year, null.ok = TRUE)
  checkmate::assert_string(station, null.ok = TRUE)
  checkmate::assert_numeric(rain_day)
  
  data_frame <- data_book$get_data_frame(data)
  if (!is.null(year)) assert_column_names(data_frame, year)
  if (!is.null(station)) assert_column_names(data_frame, station)
  if (!is.null(rain)) assert_column_names(data_frame, rain)
  
  checkmate::assert_logical(total_rain)
  checkmate::assert_logical(n_rain)
  checkmate::assert_logical(na_rm, null.ok = TRUE)
  checkmate::assert_int(na_prop, null.ok = TRUE)
  checkmate::assert_int(na_n, null.ok = TRUE)
  checkmate::assert_int(na_consec, null.ok = TRUE)
  checkmate::assert_int(na_n_non, null.ok = TRUE)
  
  
  if (!total_rain && !n_rain) {
    stop("No summaries selected. At least one of
         'total_rain' or 'n_rain' must be TRUE.")
  }
  
  columns_to_summarise <- c()
  # Create a variable which gives a 1 if it's a rain day, and 0 otherwise. 
  if (total_rain){
    columns_to_summarise <- c("rain", columns_to_summarise)
  }
  if (n_rain){
    rain_day <- instatCalculations::instat_calculation$new(type = "calculation",
                                              function_exp = paste0(rain, " >= ", rain_day),
                                              result_name = "rain_day",
                                              calculated_from = setNames(list(rain), data))
    transform_calculation <- instatCalculations::instat_calculation$new(type = "calculation",
                                              function_exp = "zoo::rollapply(data = rain_day, width = 1, FUN = sum, align = 'right', fill = NA)",
                                              result_name = "rainfall_count",
                                              sub_calculations = list(rain_day), 
                                              save = 2, # do we need to save it?
                                              before = FALSE,
                                              adjacent_column = rain)
    data_book$run_instat_calculation(calc = transform_calculation, display = FALSE)
    columns_to_summarise <- c("rainfall_count", columns_to_summarise)
  }
  
  summary_calculation(data = data,
                      date_time = date_time,
                      station = station,
                      year = year,
                      month = month,
                      to = "annual",
                      columns_to_summarise = columns_to_summarise,
                      summaries = "sum",
                      na_rm = na_rm,
                      na_prop = na_prop,
                      na_n = na_n,
                      na_consec = na_consec,
                      na_n_non = na_n_non,
                      data_book = data_book)
}
