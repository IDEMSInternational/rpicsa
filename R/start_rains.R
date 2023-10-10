#' Start of the rains
#' @description Used to present or summarise daily rainfall data in a way that corresponds to the start of the rainy season for many definitions.
#'
#' @param data The data.frame to calculate from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param rain \code{character(1)} The name of the rainfall column in \code{data} to apply the function to.
#' @param threshold \code{numerical(1)} threshold value for amount (mm) of rainfall in order to count it as a rainy day.
#' @param doy \code{character(1)} The name of the day of year column in \code{data} to apply the function to. If \code{NULL} it will be created using the \code{date_time} variable.
#' @param start_day \code{numerical(1)} The first day to calculate from in the year (1-366).
#' @param end_day \code{numerical(1)} The last day to calculate to in the year (1-366).
#' @param s_start_doy \code{numerical(1)} Default `NULL` (if `NULL`, `s_start_doy = 1`. The day of year to state is the first day of year.
#' @param output \code{character(1)} Whether to give the start of rains by day of year (doy), date, or both. Default `"doy"`.
#' @param total_rainfall \code{logical(1)} default `TRUE`. Start of the rains to be defined by the total or proportion of rainfall over a period.
#' @param over_days \code{numerical(1)} Only works if `total_rainfall = TRUE`. This is the number of days to total the rainfall over.
#' @param amount_rain \code{numerical(1)} If `total_rainfall = TRUE` and `proportion = FALSE`, the amount of rainfall to expect over the period defined in `over_days`. 
#' @param proportion \code{logical(1)} default `FALSE`, only valid if `total_rainfall = TRUE`. If `TRUE`, Start of the rains to be defined by proportion of rainfall over a period. This proportion is given in `prob_rain_day`. Otherwise, defined by the amount of rainfall over a period. The amount is given in `amount_rain`.
#' @param prob_rain_day \code{numerical(1)} Only works if `total_rainfall = TRUE` and `proportion = TRUE` This is the number 
#' @param number_rain_days \code{logical(1)} default `FALSE`. If `TRUE`, define start of the rains by the number of rainy days (`min_rain_days`) over a period. The period is given in days in the `rain_day_interval` parameter.
#' @param min_rain_days \code{numerical(1)} Only if `number_rain_days = TRUE`. This is the minimum number of rainy days to define start of rains in a given period. The period is given in days in the `rain_day_interval` parameter.
#' @param rain_day_interval \code{numerical(1)} Only if `number_rain_days = TRUE`, the interval in days that the `number_rain_days` is defined in.
#' @param dry_spell \code{logical(1)} default `FALSE`. If `TRUE`, define start of the rains by a maximum number of dry days (`spell_max_dry_days`) over a given period of days (`spell_interval`).
#' @param spell_max_dry_days \code{numerical(1)} Only if `dry_spell = TRUE`. This is the maximum number of dry days to define start of rains in a given period. The period is given in days in the `spell_interval` parameter.
#' @param spell_interval \code{numerical(1)} Only if `dry_spell = TRUE`, the interval in days that the `dry_spell` is defined in.
#' @param dry_period \code{logical(1)} default `FALSE`. If `TRUE`, define start of the rains by the maximum rain and maximum dry days in a given interval. The maximum rainfall amount is given in the `max_rain` parameter, the maximum dry days is given in the `period_max_dry_days` parameter, and the interval length is given in the `period_interval` parameter. 
#' @param max_rain \code{numerical(1)} Only if `dry_period = TRUE`, the maximum rainfall to occur in a given period.
#' @param period_max_dry_days \code{numerical(1)} Only if `dry_period = TRUE`. the maximum period of dry days to occur in a given period.
#' @param period_interval \code{numerical(1)} Only if `dry_period = TRUE`, the interval in days that the `dry_period` is defined in.
#' 
#' @return A data.frame with the date and/or day of year for the start of the rains for each year
#' @export
#'
#' @examples #TODO#
#' # check against R-Instat function
start_rains <- function(data, date_time, station = NULL, year = NULL, rain = NULL, threshold = 0.85,
                        doy = NULL, start_day = 1, end_day = 366, s_start_doy = NULL,
                        output = c("doy", "date", "both"),
                        total_rainfall = TRUE, over_days = 1, amount_rain = 20, proportion = FALSE, prob_rain_day = 0.8,
                        number_rain_days = FALSE, min_rain_days = 1, rain_day_interval = 2,
                        dry_spell = FALSE, spell_interval = 21, spell_max_dry_days = 9,
                        dry_period = FALSE, period_interval = 45, max_rain = 40, period_max_dry_days = 30) {
  
  checkmate::assert_data_frame(data)
  checkmate::assert_character(rain)
  assert_column_names(data, rain)
  checkmate::assert(checkmate::check_date(data[[date_time]], null.ok = TRUE), 
                    checkmate::check_posixct(data[[date_time]],  null.ok = TRUE))
  checkmate::assert_string(station, null.ok = TRUE)
  checkmate::assert_string(year, null.ok = TRUE)
  #checkmate::assert_string(doy, null.ok = TRUE)
  # if (!is.null(station)) assert_column_names(data, station)
  # if (!is.null(date_time)) assert_column_names(data, date_time)
  # if (!is.null(year)) assert_column_names(data, year)
  checkmate::assert_numeric(s_start_doy, lower = 1, upper = 366, null.ok = TRUE)
  checkmate::assert_logical(total_rainfall, null.ok = TRUE)
  checkmate::assert_logical(proportion, null.ok = TRUE)
  checkmate::assert_logical(number_rain_days, null.ok = TRUE)
  checkmate::assert_logical(dry_spell, null.ok = TRUE)
  checkmate::assert_logical(dry_period, null.ok = TRUE)
  checkmate::assert_number(prob_rain_day, lower = 0, upper = 1, null.ok = TRUE)
  checkmate::assert_int(start_day, lower = 1, upper = 365, null.ok = TRUE)
  checkmate::assert_int(end_day, lower = 2, upper = 366, null.ok = TRUE)
  checkmate::assert_int(over_days, lower = 1, null.ok = TRUE)
  checkmate::assert_int(amount_rain, lower = 0, null.ok = TRUE)
  checkmate::assert_int(min_rain_days, lower = 0, null.ok = TRUE)
  checkmate::assert_int(rain_day_interval, lower = 1, null.ok = TRUE)
  checkmate::assert_int(spell_interval, lower = 1, null.ok = TRUE)
  checkmate::assert_int(spell_max_dry_days, lower = 0, null.ok = TRUE)
  checkmate::assert_int(period_interval, lower = 1, null.ok = TRUE)
  checkmate::assert_int(max_rain, lower = 0, null.ok = TRUE)
  checkmate::assert_int(period_max_dry_days, lower = 0, null.ok = TRUE)
  
  if (end_day <= start_day) stop("The `end_day` must be after the `start_day`")
  if (number_rain_days){
    if (rain_day_interval < min_rain_days) stop("Value given in `rain_day_interval` must be equal to or greater than the value given in `min_rain_days`")
  }
  if (dry_spell){
    if (spell_interval < spell_max_dry_days) stop("Value given in `spell_interval` must be equal to or greater than the value given in `spell_max_dry_days`")
  }
  if (dry_period){
    if (period_interval < period_max_dry_days) stop("Value given in `period_interval` must be equal to or greater than the value given in `period_max_dry_days`")
  } 
  output <- match.arg(output)

  # Do we have a shifted start doy?
  if (!is.null(s_start_doy)){
    data <- shift_dates(data = data, date = date_time, s_start_doy = s_start_doy)
    year <- "year"
    doy <- "doy"
    data[[doy]] <- data[["s_doy"]]
    data[[year]] <- data[["s_year"]]
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

  if (!is.null(station)){
    start_of_rains <- data %>% 
      dplyr::group_by(.data[[station]], .drop = FALSE) 
  } else {
    start_of_rains <- data
  }
  # start of rains can only occur on a day that rains
  start_of_rains <- start_of_rains %>% 
    dplyr::mutate(rain_day = .data[[rain]] >= threshold)
  
  # different conditions
  if (total_rainfall){
    start_of_rains <- start_of_rains %>% 
      dplyr::mutate(roll_sum_rain = RcppRoll::roll_sumr(x = .data[[rain]], n = over_days, fill=NA, na.rm=FALSE))
    if (proportion){
      start_of_rains <- start_of_rains %>% 
        dplyr::mutate(wet_spell = stats::quantile(x=roll_sum_rain, probs = prob_rain_day, na.rm=TRUE))
    } else {
      wet_spell <- amount_rain
    }
  } else {
    start_of_rains <- start_of_rains %>% 
      dplyr::mutate(roll_sum_rain = -99)
  }
  if (number_rain_days){
    start_of_rains <- start_of_rains %>% 
      dplyr::mutate(roll_n_rain_days = RcppRoll::roll_sumr(x=rain_day, n=rain_day_interval, fill=NA, na.rm=FALSE))
  }
  if (dry_spell){
    start_of_rains <- start_of_rains %>% 
      dplyr::mutate(dry_spell = spells(x=rain_day == 0),
                    roll_max_dry_spell = dplyr::lead(x=RcppRoll::roll_maxl(x = dry_spell, n = spell_interval, fill=NA)))
  } else {
    start_of_rains <- start_of_rains %>% 
      dplyr::mutate(roll_max_dry_spell = -99)
  }
  if (dry_period){
    start_of_rains <- start_of_rains %>% 
      dplyr::mutate(roll_sum_rain_dry_period = dplyr::lead(x=RcppRoll::roll_suml(x=.data[[rain]], period_max_dry_days, fill=NA)),
                    n_dry_period = RcppRoll::roll_suml(x=roll_sum_rain_dry_period <= max_rain, n = period_interval - period_max_dry_days + 1, fill=NA, na.rm=FALSE))
  }

  # filters 
  if (total_rainfall){
    start_of_rains <- start_of_rains %>% 
      dplyr::filter(((.data[[rain]] >= threshold) & roll_sum_rain > wet_spell) | is.na(x = .data[[rain]]) | is.na(x=roll_sum_rain), .preserve = TRUE)
  }
  if (number_rain_days){
    start_of_rains <- start_of_rains %>% 
      dplyr::filter(((.data[[rain]] >= threshold) & roll_n_rain_days >= min_rain_days) | is.na(x = .data[[rain]]) | is.na(x=roll_n_rain_days), .preserve = TRUE)
  }
  if (dry_spell){
    start_of_rains <- start_of_rains %>% 
      dplyr::filter(((.data[[rain]] >= threshold) & roll_max_dry_spell <= spell_max_dry_days) | is.na(x = .data[[rain]]) | is.na(x = roll_max_dry_spell), .preserve = TRUE)
  }
  if (dry_period){
    start_of_rains <- start_of_rains %>% 
      dplyr::filter(((.data[[rain]] >= threshold) & n_dry_period == 0) | is.na(x = .data[[rain]]) | is.na(x = n_dry_period), .preserve = TRUE)
  }

    start_of_rains <- start_of_rains %>% 
      dplyr::group_by(.data[[year]], .add = TRUE, .drop = FALSE) %>% 
      dplyr::filter(.data[[doy]] >= start_day & .data[[doy]] <= end_day, .preserve = TRUE)

  # start_rains   ifelse(test=is.na(x=dplyr::first(x=rainfall)) | is.na(x=dplyr::first(x=roll_sum_rain)), yes=NA, no=dplyr::first(x=doy, default=NA))
  # start_rains_date   dplyr::if_else(condition=is.na(x=dplyr::first(x=rainfall)) | is.na(x=dplyr::first(x=roll_sum_rain)), as.Date(NA), dplyr::first(date1, default=NA))
  if (output == "doy"){
    start_of_rains <- start_of_rains %>%
      dplyr::summarise(start_rains = ifelse(is.na(x=dplyr::first(x=.data[[rain]])) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_max_dry_spell)), 
                                           NA,
                                           dplyr::first(x=.data[[doy]], default=NA)))
  } else if (output == "date") {
    start_of_rains <- start_of_rains %>%
      dplyr::summarise(start_rains = dplyr::if_else(is.na(x=dplyr::first(x=.data[[rain]])) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_max_dry_spell)), 
                                                   as.Date(NA),
                                                   dplyr::first(.data[[date_time]], default=NA)))
  } else {
    start_of_rains <- start_of_rains %>%
      dplyr::summarise(start_rains_doy = ifelse(is.na(x=dplyr::first(x=.data[[rain]])) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_max_dry_spell)), 
                                               NA,
                                               dplyr::first(x=.data[[doy]], default=NA)),
                       start_rains_date = dplyr::if_else(is.na(x=dplyr::first(x=.data[[rain]])) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_max_dry_spell)), 
                                                        as.Date(NA),
                                                        dplyr::first(.data[[date_time]], default=NA)))
  }
  return(start_of_rains)
}