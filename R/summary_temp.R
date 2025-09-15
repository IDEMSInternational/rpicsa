#' Summary Temperature (Month or annually)
#' @description Returns a summary data frame giving either the mean, minimum, and/or maximum temperatures each year from 1 Jan to 31 Dec, or by year and month.
#' 
#' @param data The data.frame to calculate from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param tmin \code{character(1)} The name of the minimum temperature column in \code{data} to apply the function to.
#' @param tmax \code{character(1)} The name of the maximum temperature column in \code{data} to apply the function to.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param month \code{character(1)} The name of the month column in \code{data}. If \code{NULL} it will be created using \code{lubridate::month(data[[date_time]])}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param to \code{character(1)} Default `annual`. The period of time to calculate the mean temperature columns over (options are `annual` or `monthly`).
#' @param summaries \code{character} The summaries to display. Options are `"mean"`, `"max"`, `"min"`.
#' @param na_rm \code{logical(1)}. Should missing values (including \code{NaN}) be removed?
#' @param na_prop \code{integer(1)} Max proportion of missing values allowed
#' @param na_n \code{integer(1)} Max number of missing values allowed
#' @param na_consec \code{integer(1)} Max number of consecutive missing values allowed
#' @param na_n_non \code{integer(1)} Min number of non-missing values required
#' @param data_book The data book object where the data object is stored, default `NULL`.
#'
#' @return A data.frame with mean summaries for each year or year and month for the minimum daily temperature and/or the maximum daily temperature.
#' @export
#'
summary_temperature <- function(data, date_time, tmin = NULL, tmax = NULL, year = NULL,
                                month = NULL, station = NULL, to = c("annual", "monthly"),
                                summaries = c("mean", "min", "max"), na_rm = FALSE,
                                na_prop = NULL, na_n = NULL, na_consec = NULL, na_n_non = NULL,
                                data_book = NULL) {
  
  if (is.null(data_book)) {
    data_book <- DataBook$new()
  }
  
  # Running checks
  checkmate::assert_string(data)
  checkmate::assert_string(date_time)
  checkmate::assert_string(tmin, null.ok = TRUE)
  checkmate::assert_string(tmax, null.ok = TRUE)
  checkmate::assert_string(year, null.ok = TRUE)
  checkmate::assert_string(month, null.ok = TRUE)
  checkmate::assert_string(station, null.ok = TRUE)
  data_frame <- data_book$get_data_frame(data)
  assert_column_names(data_frame, date_time)
  checkmate::assert(checkmate::check_date(data_frame[[date_time]], null.ok = TRUE), 
                    checkmate::check_posixct(data_frame[[date_time]],  null.ok = TRUE))
  if (!is.null(tmin)) assert_column_names(data_frame, tmin)
  if (!is.null(tmax)) assert_column_names(data_frame, tmax)
  if (!is.null(year)) assert_column_names(data_frame, year)
  if (!is.null(rain)) assert_column_names(data_frame, month)
  if (!is.null(station)) assert_column_names(data_frame, station)
  checkmate::assert_logical(na_rm, null.ok = TRUE)
  checkmate::assert_int(na_prop, null.ok = TRUE)
  checkmate::assert_int(na_n, null.ok = TRUE)
  checkmate::assert_int(na_consec, null.ok = TRUE)
  checkmate::assert_int(na_n_non, null.ok = TRUE)
  
  to <- match.arg(to)
  if (is.null(tmin) && is.null(tmax)) { stop("At least one of 'tmin' or 'tmax' must be provided.") }
  
  # specifying the columns to summarize
  columns_to_summarise <- c(tmin, tmax)
  columns_to_summarise <- columns_to_summarise[!sapply(columns_to_summarise, is.null)]
  
  summary_calculation(data = data, date_time = date_time, year = year, month = month,
                      station = station, to = to, columns_to_summarise = columns_to_summarise,
                      summaries = summaries, na_rm = na_rm, na_prop = na_prop, na_n = na_n,
                      na_consec = na_consec, na_n_non = na_n_non, data_book = data_book)
}