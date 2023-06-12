#' Season start date probabilities
#' @description The probabilities of the start of the rains occurring on or before a set of specified days.
#'
#' @param data The data.frame to calculate from that contains the `start_rains` variable.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param start_rains \code{character(1)} The name of the start of rains column in \code{data}. This can be calculated by the `start_rains` function.
#' @param doy_format \code{character(1)} Whether `start_rains` is given as `1-366` (`"doy_366"`) or `1-365` (`"doy_365"`), if `doy` is given instead of `date`. If a `date` is given, this is the value to convert to, and hence, the value that `specified_day` takes as well.
#' @param specified_day \code{character}. A character vector containing specified doys to calculate the probability of rain before. This is assumed to follow the same as start of rains (see `doy_format`).
#' 
#' @return Returns a summary table giving the probability that the start of rains will occur on or before the day of years specified.
#' @export
#'
#' @examples #TODO#
#' #x <- start_rains(daily_niger, date_time = "date", station = "station_name", rain = "rain")
#' #probability_season_start(x, station = "station_name", start_rains = "start_rain", specified_day = c(150, 200))

probability_season_start <- function(data, station = NULL, start_rains, doy_format = c("doy_366", "doy_365"), # assumed to be 366 then.
                                     specified_day) {
  doy_format <- match.arg(doy_format)
  
  # we check for format in SOR
  # transform date variable to doy for the specified day
  if (lubridate::is.Date(data[[start_rains]])){
    if (doy_format == "366"){ # if doy for specified doy is 366, then set start_rains as 366 doy
      data[[start_rains]] <- cdms.products::yday_366(as.Date(data[[start_rains]]))
    } else {
      data[[start_rains]] <- lubridate::yday(as.Date(data[[start_rains]]))
    }
  }

  # repeat data frame to include specified day
  # or repeat below for each specified day
  if (!is.null(station)){
    summary_proportion <- purrr::map_df(.x = specified_day,
                                        .f =~ data %>% dplyr::mutate(LT = ifelse(.data[[start_rains]] < .x, 1, 0)) %>%
                                          dplyr::group_by(.data[[station]], .drop = FALSE) %>%
                                          dplyr::summarise(value = .x, proportion = sum(LT, na.rm = TRUE)/dplyr::n()))
    summary_proportion <- summary_proportion %>%
      tidyr::pivot_wider(id_cols = .data[[station]], names_from = value, values_from = proportion, names_prefix = "proportion_")
  } else {
    summary_proportion <- purrr::map_df(.x = specified_day,
                                        .f =~ data %>% dplyr::mutate(LT = ifelse(.data[[start_rains]] < .x, 1, 0)) %>%
                                          dplyr::summarise(value = .x, proportion = sum(LT, na.rm = TRUE)/dplyr::n()))
    #dplyr::summarise("proportion_{.x}" := sum(LT, na.rm = TRUE)/dplyr::n()))
    summary_proportion <- summary_proportion %>%
      tidyr::pivot_wider(names_from = value, values_from = proportion, names_prefix = "proportion_")
  }
  return(summary_proportion)
}