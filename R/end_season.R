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
#' @param capacity \code{numerical(1)} Water capacity of the soil (default `60`).
#' @param water_balance_max \code{numerical(1)} Maximum water balance value (default `0.5`).
#' @param evaporation \code{character(1)} Whether to give evaporation as a value or variable. Default `"value"`.
#' @param evaporation_value \code{numerical(1)} If `evaporation = "value"`, the numerical value of amount of evaporation per day (default `5`).
#' @param evaporation_variable \code{character(1)} If `evaporation = "variable"`, the variable in `data` that corresponds to the evaporation column.
#'
#' @return A data.frame with the day of year and/or date for the end of the season for each year (and station).
#' @export
#'
#' @examples #TODO # is same as R-Instat
#' # end_season(niger_daily_1, date_time = "date", year = "year",
#' #rain = "rain", doy = "doy", output = "doy",
#' #capacity = 60, water_balance_max = 0.5,
#' #evaporation_value = 5)
end_season <- function(data, date_time, station = NULL, year = NULL, rain = NULL,
                       doy = NULL,  s_start_doy = NULL,
                       start_day = 1, end_day = 366, output = c("doy", "date", "both"),
                       capacity = 60, water_balance_max = 0.5, evaporation = c("value", "variable"),
                       evaporation_value = 5, evaporation_variable = NULL){
  # TODO: set up evaporation_variable
  checkmate::assert_data_frame(data)
  checkmate::assert_character(rain)
  assert_column_names(data, rain)
  checkmate::assert(checkmate::check_date(data[[date_time]], null.ok = TRUE), 
                    checkmate::check_posixct(data[[date_time]],  null.ok = TRUE))
  checkmate::assert_string(station, null.ok = TRUE)
  checkmate::assert_string(year, null.ok = TRUE)
  checkmate::assert_string(doy, null.ok = TRUE)
  checkmate::assert_int(start_day, lower = 1, upper = 365)
  checkmate::assert_int(end_day, lower = 2, upper = 366)
  checkmate::assert_numeric(capacity, lower = 0)
  checkmate::assert_numeric(water_balance_max, lower = 0)
  checkmate::assert_numeric(evaporation_value, lower = 0)
  checkmate::assert_numeric(s_start_doy, lower = 1, upper = 366, null.ok = TRUE)
  output <- match.arg(output)
  evaporation <- match.arg(evaporation)
  if (end_day <= start_day) stop("The `end_day` must be after the `start_day`")
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
  
  # Create variables for WB code
  data <- data %>%
    # create rain_min with NA as 0
    dplyr::mutate(rain_min = ifelse(is.na(.data[[rain]]), 0, .data[[rain]])) %>%
    dplyr::mutate(wb_min = Reduce(f = function(x, y) pmin(pmax(x + y, 0), capacity), x = utils::tail(x=rain_min - evaporation_value, n=-1), init=0, accumulate=TRUE)) %>%
    dplyr::mutate(rain_max = ifelse(is.na(.data[[rain]]), capacity, .data[[rain]])) %>%
    dplyr::mutate(wb_max = Reduce(f = function(x, y) pmin(pmax(x + y, 0), capacity), x = utils::tail(x=rain_max - evaporation_value, n=-1), init=0, accumulate=TRUE)) %>%
    dplyr::mutate(wb = ifelse((wb_min != wb_max) | is.na(.data[[rain]]), NA, wb_min))
  if (!is.null(station)){
    end_of_season <- data %>% 
      dplyr::group_by(.data[[station]], .add = TRUE, .drop = FALSE) 
  } else {
    end_of_season <- data
  }
  
  end_of_season <- end_of_season %>%
    dplyr::filter(wb <= water_balance_max | is.na(.data[[rain]])) %>%
    dplyr::group_by(.data[[year]], .add = TRUE, .drop = FALSE) %>%
    dplyr::filter(.data[[doy]] >= start_day & .data[[doy]] <= end_day, .preserve = TRUE)
  
  if (output == "doy"){
    end_of_season <- end_of_season %>%
      dplyr::summarise(end_season = ifelse(is.na(x=dplyr::first(wb)),
                                           NA,
                                           dplyr::first(.data[[doy]])))
    
  } else if (output == "date") {
    end_of_season <- end_of_season %>%
      dplyr::summarise(end_season = dplyr::if_else(is.na(x=dplyr::first(wb)),
                                                   as.Date(NA),
                                                   dplyr::first(.data[[date_time]])))
  } else {
    end_of_season <- end_of_season %>%
      dplyr::summarise(end_season = ifelse(is.na(x=dplyr::first(wb)),
                                           NA,
                                           dplyr::first(.data[[doy]])),
                       end_season_date = dplyr::if_else(is.na(x=dplyr::first(wb)),
                                                   as.Date(NA),
                                                   dplyr::first(.data[[date_time]])))
  }
  return(end_of_season)
}
