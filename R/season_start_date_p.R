#' Season start date probabilities
#' @description The probabilities of the start of the rains occurring on or before a set of specified dates.
#'
#' @param data The data.frame to calculate from that contains the `start_rains` variable.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param start_rains \code{character(1)} The name of the start of rains column in \code{data}. This can be calculated by the `start_rains` function.
#' @param format_SOR \code{character(1)} Whether `start_rains` is given as `1-366` or `1-365`, if `doy` is given instead of `date`.
#' @param specified_day \code{\link[base]{Date(1)}} or \code{character(1)}. The specified date or day-of-year to calculate the probability of rain before. If a doy value is given, it is assumed to follow the same format as the value given in `format_SOR`.
#' @param format_day \code{character(1)} Whether the value given in `specified_day` is the `date`, `doy` (day of year 1-366), or `doy_365` (day of year 1-365) value.
#' @param date_format \code{character(1)} The date format as used by `base::strptime`.
#' 
#' @return Returns a summary table giving the probability that the start of rains will occur on or before the date specified.
#' @export
#'
#' @examples #TODO#
#' #x <- start_rains(daily_niger, date_time = "date", station = "station_name", rain = "rain")
#' #season_start_date_p(x, station = "station_name", start_rains = "start_rain", specified_day = 150,
#' #                    format_day = "doy")
season_start_date_p <- function(data, station = NULL, start_rains, format_SOR = c("366", "365"), specified_day, format_day = c("date", "doy", "doy_365"), date_format = "%Y-%m-%d") {
  format_SOR <- match.arg(format_SOR)
  format_day <- match.arg(format_day)
  
  # transform date variable to doy for the specified day
  if (format_day == "date"){
    if (format_SOR == "366"){
      specified_day <- cdms.products::yday_366(as.Date(specified_day, format = date_format))
    } else {
      specified_day <- yday(as.Date(specified_day, format = date_format))
    }
  } else if (format_day == "doy_365" && format_SOR == "366"){
    if (specified_day > 59) specified_day <- specified_day + 1
    specified_day <- cdms.products::yday_366(as.Date(specified_day, format = date_format))
  } else if (format_day == "doy" && format_SOR == "365"){
    if (specified_day > 59) specified_day <- specified_day - 1
  }
  # transform date variable to doy for the start_rains (the same doy 1-366 as SOR)
  if (is.Date(data[[start_rains]])){
    if (format_SOR == "366"){
      .data[[start_rains]] <- cdms.products::yday_366(as.Date(data[[start_rains]], format = date_format))
    } else {
      .data[[start_rains]] <- yday(as.Date(data[[start_rains]], format = date_format))
    }
  }
  data <- data %>%
    dplyr::mutate(LT = ifelse(.data[[start_rains]] < specified_day, 1, 0))
  if (is.null(data[[station]])){
    summary_proportion <- data %>%
      dplyr::group_by(.data[[station]], .drop = FALSE) %>%
      dplyr::mutate(total_n = n()) %>%
      dplyr::summarise(proportion = sum(LT, na.rm = TRUE)/total_n)
  } else {
    summary_proportion <- data %>%
      dplyr::summarise(proportion = sum(LT, na.rm = TRUE)/n())
  }
  return(summary_proportion)
}