#' Start of the rains
#'
#' @param data The data.frame to calculate from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param rain \code{character(1)} The name of the rainfall column in \code{data} to apply the function to.
#' @param threshold \code{numerical(1)} Threshold value for amount (mm) of rainfall in order to count it as a rainy day.
#' @param doy \code{character(1)} The name of the day of year column in \code{data} to apply the function to. If \code{NULL} it will be created using the \code{date_time} variable.
#' @param start_day \code{numerical(1)} The first day to calculate from in the year (1-366).
#' @param end_day \code{numerical(1)} The last day to calculate to in the year (1-366).
#' @param total_rainfall 
#' @param over_days 
#' @param amount_rain 
#' @param proportion 
#' @param prob_rain_day 
#' @param number_rain_days 
#' @param min_rain_days 
#' @param out_of_rain_days 
#' @param dry_spell 
#' @param spell_interval 
#' @param spell_max_dry_days 
#' @param dry_period 
#' @param period_interval 
#' @param max_rain 
#' @param period_max_dry_days 
#'
#' @return A data.frame with the date of the start of the rains for each year
#' @export
#'
#' @examples #TODO#
#' 
#' # TODO: add in skeleton function explanations
#' # check against R-Instat function
#' # read in "spells" function (cdms.products?)
#' # TODO: option for doy, date, etc
#' # TODO: checkmate:: functions
start_rains <- function(data, date_time, station = NULL, year = NULL, rain = NULL, threshold = 0.85,
                        doy = NULL, start_day = 1, end_day = 366,
                        total_rainfall = TRUE, over_days = 1, amount_rain = 20, proportion = FALSE, prob_rain_day = 0.8,
                        number_rain_days = FALSE, min_rain_days = 1, out_of_rain_days = 2,
                        dry_spell = FALSE, spell_interval = 21, spell_max_dry_days = 9,
                        dry_period = FALSE, period_interval = 45, max_rain = 40, period_max_dry_days = 30) {
  
  # calculate doy, year from date
  if(is.null(year)) {
    year <- "year"
    data[[year]] <- lubridate::year(data[[date_time]])
  }
  if(is.null(doy)) {
    doy <- "doy"
    data[[doy]] <- cdms.products::yday_366(data[[date_time]])
  }
  if (!is.null(station)){
    start_of_rains <- data %>% 
      dplyr::group_by(.data[[station]]) 
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
        mutate(wet_spell = quantile(x=roll_sum_rain, probs = prob_rain_day, na.rm=TRUE))
    } else {
      wet_spell <- amount_rain
    }
  } else {
    start_of_rains <- start_of_rains %>% 
      dplyr::mutate(roll_sum_rain = -99)
  }
  View(head(start_of_rains))
  if (number_rain_days){
    start_of_rains <- start_of_rains %>% 
      dplyr::mutate(roll_n_rain_days = RcppRoll::roll_sumr(x=rain_day, n=out_of_rain_days, fill=NA, na.rm=FALSE))
  }
  if (dry_spell){
    start_of_rains <- start_of_rains %>% 
      dplyr::mutate(dry_spell = .spells(x=rain_day == 0),
                    roll_max_dry_spell = dplyr::lead(x=RcppRoll::roll_maxl(x = dry_spell, n = spell_interval, fill=NA)))
  } else {
    start_of_rains <- start_of_rains %>% 
      dplyr::mutate(roll_max_dry_spell = -99)
  }
  if (dry_period){
    start_of_rains <- start_of_rains %>% 
      dplyr::mutate(roll_sum_rain_dry_period = lead(x=RcppRoll::roll_suml(x=.data[[rain]], period_max_dry_days, fill=NA)),
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
    dplyr::group_by(.data[[year]], .add = TRUE) %>%
    dplyr::filter(.data[[doy]] >= start_day & .data[[doy]] <= end_day, .preserve = TRUE) %>%
    dplyr::summarise(start_rain = #ifelse(is.na(x=dplyr::first(x=.data[[rain]])) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_max_dry_spell)), 
                                  #           NA,
                                             dplyr::first(x=.data[[doy]], default=NA))#)
  
  return(start_of_rains)
}
