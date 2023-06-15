#' Seasonal total rainfall
#' @description Total annual rainfall between start of the rains and end of the season.
#' 
#' @inheritParams annual_rain
#' 
#' @param summary_data Summary data frame containing the `start_date` and `end_date` variables. These variables are calculated from start of rains and end of season functions.
#' If `NULL`, `start_date` and `end_date` are calculated from the `start_of_rains` and `end_of_season` functions respectively.
#' @param start_date \code{character(1)} The name of the start of rains column in \code{summary_data}. If \code{NULL} it will be created using the \code{start_of_rains} function.
#' @param end_date \code{character(1)} The name of the end of season column in \code{summary_data}. If \code{NULL} it will be created using the \code{end_of_seasons} function.
#' @param data The daily data.frame to calculate rainfall from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param rain \code{character(1)} The name of the rainfall column in \code{data} to apply the function to.
#' @param doy \code{character(1)} The name of the day of year column in \code{data} to apply the function to. If \code{NULL} it will be created using the \code{date_time} variable.
#' @param threshold \code{numerical(1)} threshold value for amount (mm) of rainfall in order to count it as a rainy day.
#' @param total_rain \code{logical(1)} default `TRUE`. Display the total rainfall value for each year.
#' @param n_rain \code{logical(1)} default `TRUE`. Display the number of rainfall days.
#' @param rain_day \code{numerical(1)} If `n_rain = TRUE`, the minimum rainfall value in a day for that day to count as a rainfall day.
#' @param na_rm \code{logical(1)}. Should missing values (including \code{NaN}) be removed?
#' @param na_prop \code{integer(1)} Max proportion of missing values allowed
#' @param na_n \code{integer(1)} Max number of missing values allowed
#' @param na_consec \code{integer(1)} Max number of consecutive missing values allowed
#' @param na_n_non \code{integer(1)} Min number of non-missing values required
#' @param sor_start_day \code{numerical(1)} The first day to calculate from in the year (1-366).
#' @param sor_end_day \code{numerical(1)} The last day to calculate to in the year (1-366).
#' @param sor_total_rainfall \code{logical(1)} default `TRUE`. Start of the rains to be defined by the total or proportion of rainfall over a period.
#' @param sor_over_days \code{numerical(1)} Only works if `total_rainfall = TRUE`. This is the number of days to total the rainfall over.
#' @param sor_amount_rain \code{numerical(1)} If `total_rainfall = TRUE` and `proportion = FALSE`, the amount of rainfall to expect over the period defined in `over_days`. 
#' @param sor_proportion \code{logical(1)} default `FALSE`, only valid if `total_rainfall = TRUE`. If `TRUE`, Start of the rains to be defined by proportion of rainfall over a period. This proportion is given in `prob_rain_day`. Otherwise, defined by the amount of rainfall over a period. The amount is given in `amount_rain`.
#' @param sor_prob_rain_day \code{numerical(1)} Only works if `total_rainfall = TRUE` and `proportion = TRUE` This is the number 
#' @param sor_number_rain_days \code{logical(1)} default `FALSE`. If `TRUE`, define start of the rains by the number of rainy days (`min_rain_days`) over a period. The period is given in days in the `rain_day_interval` parameter.
#' @param sor_min_rain_days \code{numerical(1)} Only if `number_rain_days = TRUE`. This is the minimum number of rainy days to define start of rains in a given period. The period is given in days in the `rain_day_interval` parameter.
#' @param sor_rain_day_interval \code{numerical(1)} Only if `number_rain_days = TRUE`, the interval in days that the `number_rain_days` is defined in.
#' @param sor_dry_spell \code{logical(1)} default `FALSE`. If `TRUE`, define start of the rains by a maximum number of dry days (`spell_max_dry_days`) over a given period of days (`spell_interval`).
#' @param sor_spell_max_dry_days \code{numerical(1)} Only if `dry_spell = TRUE`. This is the maximum number of dry days to define start of rains in a given period. The period is given in days in the `spell_interval` parameter.
#' @param sor_spell_interval \code{numerical(1)} Only if `dry_spell = TRUE`, the interval in days that the `dry_spell` is defined in.
#' @param sor_dry_period \code{logical(1)} default `FALSE`. If `TRUE`, define start of the rains by the maximum rain and maximum dry days in a given interval. The maximum rainfall amount is given in the `max_rain` parameter, the maximum dry days is given in the `period_max_dry_days` parameter, and the interval length is given in the `period_interval` parameter. 
#' @param sor_max_rain \code{numerical(1)} Only if `dry_period = TRUE`, the maximum rainfall to occur in a given period.
#' @param sor_period_max_dry_days \code{numerical(1)} Only if `dry_period = TRUE`. the maximum period of dry days to occur in a given period.
#' @param sor_period_interval \code{numerical(1)} Only if `dry_period = TRUE`, the interval in days that the `dry_period` is defined in.
#' @param end_type \code{character(1)} If `is.null(end_date)`, `end_type` is whether the end of seasons or end of rains is used. Options are c(`"season", "rains"`), default `"season"`.
#' @param eos_start_day \code{numerical(1)} The first day to calculate from in the year (1-366).
#' @param eos_end_day \code{numerical(1)} The last day to calculate to in the year (1-366).
#' @param eor_interval_length \code{numerical(1)} Number of days for the minimum rainfall to fall in.
#' @param eor_min_rainfall \code{numerical(1)} Minimum amount of rainfall to occur on the set of days  defined in `interval_length`.
#' @param eos_capacity \code{numerical(1)} Water capacity of the soil (default `60`).
#' @param eos_water_balance_max \code{numerical(1)} Maximum water balance value (default `0.5`).
#' @param eos_evaporation \code{character(1)} Whether to give evaporation as a value or variable. Default `"value"`.
#' @param eos_evaporation_value \code{numerical(1)} If `evaporation = "value"`, the numerical value of amount of evaporation per day (default `5`).
#' @param eos_evaporation_variable \code{character(1)} If `evaporation = "variable"`, the variable in `data` that corresponds to the evaporation column.

#' @return A data.frame with rainfall summaries for each year in the specified season (between start of the rains and end of season).
#' @export
#'
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @examples # TODO
#' library(cdms.products)
#' data(daily_niger)
#' 
#' seasonal_rain(data = daily_niger, station = "station_name", date_time = "date",
#' year = "year", doy = "doy", rain = "rain")
#' 
#' # Using data
#' start_output <- start_rains(data = daily_niger, station = "station_name", date_time = "date",
#'                             year = "year", doy = "doy", rain = "rain")
#' end_output <- end_rains(data = daily_niger, station = "station_name", date_time = "date",
#'                         year = "year", doy = "doy", rain = "rain")
#' summary_data <- dplyr::full_join(start_output, end_output)
#' start_output <- seasonal_rain(summary_data = summary_data, date_time = "date", station = "station_name", data = daily_niger, year = "year", start_date = "start_rain", end_date = "end_rain", rain = "rain")
#' 
#' # Or to create one of the start rains in the function:
#' summary_data <- end_output
#' start_output <- seasonal_rain(summary_data = summary_data, date_time = "date", station = "station_name", data = daily_niger, year = "year", end_date = "end_rain", rain = "rain", threshold = 20)

seasonal_rain <- function(summary_data = NULL, start_date = NULL, end_date = NULL,
                          data, date_time, year = NULL, station = NULL, doy = NULL,
                          rain = NULL,
                          total_rain = TRUE, n_rain = TRUE, rain_day = 0.85,
                          na_rm = FALSE, na_prop = NULL, na_n = NULL, 
                          na_consec = NULL, na_n_non = NULL,
                          # start of rains parameters
                          threshold = 0.85, sor_start_day = 1, sor_end_day = 366,
                          sor_total_rainfall = TRUE, sor_over_days = 1, sor_amount_rain = 20, sor_proportion = FALSE, sor_prob_rain_day = 0.8,
                          sor_number_rain_days = FALSE, sor_min_rain_days = 1, sor_rain_day_interval = 2,
                          sor_dry_spell = FALSE, sor_spell_interval = 21, sor_spell_max_dry_days = 9,
                          sor_dry_period = FALSE, sor_period_interval = 45, sor_max_rain = 40, sor_period_max_dry_days = 30,
                          # end of rains parameters
                          end_type = c("season", "rains"),
                          eos_start_day = 1, eos_end_day = 366, eor_interval_length = 1, eor_min_rainfall = 10,
                          eos_capacity = 60, eos_water_balance_max = 0.5, eos_evaporation = c("value", "variable"),
                          eos_evaporation_value = 5, eos_evaporation_variable = NULL)
{
  end_type <- match.arg(end_type)
  if(is.null(doy)) {
    doy <- "doy"
    data <- data %>% dplyr::mutate(doy != cdms.products::yday_366(.data[[date_time]]))
  }
  if(is.null(year)) {
    data[["year"]] <- lubridate::year(data[[year]])
  }
  if (is.null(summary_data)){
    summary_data <- tidyr::crossing(!!station := unique(data[[station]]), !!year := unique(data[[year]]))
  }
  if (is.null(start_date)){
    start_rains_data <- start_rains(data = data, date_time = date_time, station = station, year = year, rain = rain, threshold = threshold,
                         doy = doy, start_day = sor_start_day, end_day = sor_end_day, output = "doy",
                         total_rainfall = sor_total_rainfall, over_days = sor_over_days, amount_rain = sor_amount_rain, proportion = sor_proportion, prob_rain_day = sor_prob_rain_day,
                         number_rain_days = sor_number_rain_days, min_rain_days = sor_min_rain_days, rain_day_interval = sor_rain_day_interval,
                         dry_spell = sor_dry_spell, spell_interval = sor_spell_interval, spell_max_dry_days = sor_spell_max_dry_days,
                         dry_period = sor_dry_period, period_interval = sor_period_interval, max_rain = sor_max_rain, period_max_dry_days = sor_period_max_dry_days)
    start_date <- "start_rain"
    summary_data <- dplyr::full_join(summary_data, start_rains_data)
  }
  if (is.null(end_date)){
    if (end_type == "rains"){
      end_rains_data <- end_rains(data = data, date_time = date_time, station = station, year = year, rain = rain,
                                  doy = doy, start_day = eos_start_day, end_day = eos_end_day, output = "doy",
                                  interval_length = eor_interval_length, min_rainfall = eor_min_rainfall) 
    } else {
      end_rains_data <- end_season(data = data, date_time = date_time, station = station, year = year, rain = rain,
                                   doy = doy, start_day = eos_start_day, end_day = eos_end_day, output = "doy",
                                   capacity = eos_capacity, water_balance_max = eos_water_balance_max,
                                   evaporation = eos_evaporation, evaporation_value = eos_evaporation_value,
                                   evaporation_variable = eos_evaporation_variable)
    }
    end_date <- "end_rain"
    summary_data <- dplyr::full_join(summary_data, end_rains_data)
    }
  if (!total_rain && !n_rain) {
    stop("No summaries selected. At least one of
         'total_rain' or 'n_rain' must be TRUE.")
  }
  
  summary_data <- dplyr::full_join(data %>% dplyr::select(c({{ station }}, {{ year }}, {{ date_time }}, {{ doy }}, {{ rain }})), summary_data)
  summary_data <- summary_data %>% dplyr::group_by(.data[[station]], .data[[year]])
  if (lubridate::is.Date(summary_data[[start_date]])){
    summary_data <- summary_data %>% 
      dplyr::filter(.data[[date_time]] >= .data[[start_date]])
  } else {
    summary_data <- summary_data %>%
      dplyr::filter(.data[[doy]] >= .data[[start_date]])
  }
  if (lubridate::is.Date(summary_data[[end_date]])){
    summary_data <- summary_data %>%
      dplyr::filter(.data[[date_time]] <= .data[[end_date]])
  } else {
    summary_data <- summary_data %>%
      dplyr::filter(.data[[doy]] <= .data[[end_date]])
  }

  summaries <- c()
  if (total_rain) summaries <- c(total_rain = "sum")
  if (n_rain) summaries <- c(summaries, 
                             n_rain = paste0("~sum(.x > ", rain_day, ")"))
  climatic_output <- cdms.products::climatic_summary(data = summary_data, date_time = date_time, 
                                  station = station, elements = rain,
                                  year = year, to = "annual", 
                                  summaries = summaries, na_rm = na_rm, 
                                  na_prop = na_prop, na_n = na_n, 
                                  na_n_non = na_n_non, names = "{.fn}")
  return(climatic_output)
}
