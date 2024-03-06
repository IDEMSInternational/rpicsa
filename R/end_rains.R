#' End of the rains
#' @description Last occurrence from day q with at least z mm rainfall on a set of days.
#'
#' @param data The data.frame to calculate from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param rain \code{character(1)} The name of the rainfall column in \code{data} to apply the function to.
#' @param doy \code{character(1)} The name of the day of year column in \code{data} to apply the function to. If \code{NULL} it will be created using the \code{date_time} variable.
#' @param s_start_doy \code{numerical(1)} Default `NULL` (if `NULL`, `s_start_doy = 1`. The day of year to state is the first day of year.
#' @param start_day \code{numerical(1)} The first day to calculate from in the year (1-366).
#' @param end_day \code{numerical(1)} The last day to calculate to in the year (1-366).
#' @param output \code{character(1)} Whether to give the start of rains by day of year (doy), date, or both. Default `"doy"`.
#' @param interval_length \code{numerical(1)} Number of days for the minimum rainfall to fall in.
#' @param min_rainfall \code{numerical(1)} Minimum amount of rainfall to occur on the set of days defined in `interval_length`.
#'
#' @return A data.frame with the day of year and/or date for the end of the rains for each year (and station).
#' @export
#'
#' @examples #TODO#
#' # is same as R-Instat
end_rains <- function(data, date_time, station = NULL, year = NULL, rain = NULL,
                      doy = NULL,  s_start_doy = NULL,
                      start_day = 1, end_day = 366, output = c("doy", "date", "both"),
                      interval_length = 1, min_rainfall = 10){
  
  checkmate::assert_data_frame(data)
  checkmate::assert_character(rain)
  assert_column_names(data, rain)
  checkmate::assert(checkmate::check_date(data[[date_time]], null.ok = TRUE), 
                    checkmate::check_posixct(data[[date_time]],  null.ok = TRUE))
  checkmate::assert_string(station, null.ok = TRUE)
  checkmate::assert_string(year, null.ok = TRUE)
  checkmate::assert_string(doy, null.ok = TRUE)
  checkmate::assert_numeric(s_start_doy, lower = 1, upper = 366, null.ok = TRUE)
  # if (!is.null(station)) assert_column_names(data, station)
  # if (!is.null(date_time)) assert_column_names(data, date_time)
  # if (!is.null(year)) assert_column_names(data, year)
  # if (!is.null(doy)) assert_column_names(data, doy)
  checkmate::assert_int(start_day, lower = 1, upper = 365)
  checkmate::assert_int(end_day, lower = 2, upper = 366)
  checkmate::assert_int(interval_length, lower = 1)
  checkmate::assert_int(min_rainfall, lower = 0)
  if (end_day <= start_day) stop("The `end_day` must be after the `start_day`")
  output <- match.arg(output)
  
  # Do we have a shifted start doy?
  if (!is.null(s_start_doy)){
    data <- shift_dates(data = data, date = date_time, s_start_doy = s_start_doy - 1)
    year <- "year"
    doy <- "doy"
    data[[year]] <- data[["s_year"]]
    data[[doy]] <- data[["s_doy"]]
  } else {
    # calculate doy, year from date
    if(is.null(year)){#if(!year %in% names(data)) { # do instead of is.null because of epicsawrap. we always read in "year" whether it exists or not.
      year <- "year"
      data[[year]] <- lubridate::year(data[[date_time]])
    }
    if(is.null(doy)){ #(!doy %in% names(data)) {
      doy <- "doy"
      data[[doy]] <- yday_366(data[[date_time]])
    }
  }
  # to avoid dropping levels, set as factor
  data[[year]] <- factor(data[[year]])
  if (!is.null(station)){
    end_of_rains <- data %>% 
      dplyr::group_by(.data[[station]], .drop = FALSE) 
  } else {
    end_of_rains <- data
  }
  
  end_of_rains <- end_of_rains %>%
    dplyr::mutate(roll_sum_rain = RcppRoll::roll_sumr(x = .data[[rain]], n = interval_length, fill = NA, na.rm = FALSE)) %>%
    dplyr::filter((roll_sum_rain > min_rainfall) | is.na(x=roll_sum_rain)) %>% 
    dplyr::group_by(.data[[year]], .add = TRUE, .drop = FALSE) %>%
    dplyr::filter(.data[[doy]] >= start_day & .data[[doy]] <= end_day, .preserve = TRUE)
  
  if (output == "doy"){
    end_of_rains <- end_of_rains %>%
      dplyr::summarise(end_rains = ifelse(is.na(x = dplyr::last(x = roll_sum_rain)), 
                                         NA, 
                                         dplyr::last(x=.data[[doy]], default=NA)))
    
  } else if (output == "date") {
    end_of_rains <- end_of_rains %>%
      dplyr::summarise(end_rains = dplyr::if_else(is.na(x = dplyr::last(x = roll_sum_rain)), 
                                                 as.Date(NA),
                                                 dplyr::last(.data[[date_time]], default=NA)))
  } else {
    end_of_rains <- end_of_rains %>%
      dplyr::summarise(end_rains_doy = ifelse(is.na(x = dplyr::last(x = roll_sum_rain)), 
                                             NA, 
                                             dplyr::last(x=.data[[doy]], default=NA)),
                       end_rains_date = dplyr::if_else(is.na(x = dplyr::last(x = roll_sum_rain)), 
                                                      as.Date(NA),
                                                      dplyr::last(.data[[date_time]], default=NA)))
  }
  return(end_of_rains)
}
