#' Length of the season
#' @description Number of days between start of the rains and end of the season
#'
#' @inheritParams seasonal_rain
#'
#' @return A data.frame with length of rainfall season for each year in the specified season (between start of the rains and end of season).
#' @export
#'
#' @examples # Example of season
#' library(cdms.products)
#' data(daily_niger)
#' 
#' seasonal_length(data = daily_niger, station = "station_name", date_time = "date",
#' year = "year", doy = "doy", rain = "rain")
#' 
#' # Using data
#' start_output <- start_rains(data = daily_niger, station = "station_name", date_time = "date",
#'                             year = "year", doy = "doy", rain = "rain")
#' end_output <- end_rains(data = daily_niger, station = "station_name", date_time = "date",
#'                         year = "year", doy = "doy", rain = "rain")
#' summary_data <- dplyr::full_join(start_output, end_output)
#' start_output <- seasonal_length(summary_data = summary_data, start_date = "start_rain", end_date = "end_rain")
seasonal_length <- function(summary_data = NULL, start_date = NULL, end_date = NULL,
                          data, date_time, rain, year = NULL, station = NULL, doy = NULL, 
                          # start of rains parameters
                          threshhold = 0.85, sor_start_day = 1, sor_end_day = 366,
                          sor_total_rainfall = TRUE, sor_over_days = 1, sor_amount_rain = 20, sor_proportion = FALSE, sor_prob_rain_day = 0.8,
                          sor_number_rain_days = FALSE, sor_min_rain_days = 1, sor_rain_day_interval = 2,
                          sor_dry_spell = FALSE, sor_spell_interval = 21, sor_spell_max_dry_days = 9,
                          sor_dry_period = FALSE, sor_period_interval = 45, sor_max_rain = 40, sor_period_max_dry_days = 30,
                          # end of rains parameters
                          eos_start_day = 1, eos_end_day = 366, eos_interval_length = 1, eos_min_rainfall = 10)
{
  
  if (is.null(summary_data)) summary_data <- tidyr::crossing(!!station := unique(data[[station]]), !!year := unique(data[[year]]))
  if (is.null(start_date)){
    start_rains_data <- start_rains(data = data, date_time = date_time, station = station, year = year, rain = rain, threshhold = threshhold,
                                    doy = doy, start_day = sor_start_day, end_day = sor_end_day, output = "doy",
                                    total_rainfall = sor_total_rainfall, over_days = sor_over_days, amount_rain = sor_amount_rain, proportion = sor_proportion, prob_rain_day = sor_prob_rain_day,
                                    number_rain_days = sor_number_rain_days, min_rain_days = sor_min_rain_days, rain_day_interval = sor_rain_day_interval,
                                    dry_spell = sor_dry_spell, spell_interval = sor_spell_interval, spell_max_dry_days = sor_spell_max_dry_days,
                                    dry_period = sor_dry_period, period_interval = sor_period_interval, max_rain = sor_max_rain, period_max_dry_days = sor_period_max_dry_days)
    summary_data <- dplyr::full_join(summary_data, start_rains_data)
  }
  if (is.null(end_date)){
    end_rains_data <- end_rains(data = data, date_time = date_time, station = station, year = year, rain = rain,
                                doy = doy, start_day = eos_start_day, end_day = eos_end_day, output = "doy",
                                interval_length = eos_interval_length, min_rainfall = eos_min_rainfall)
    summary_data <- dplyr::full_join(summary_data, end_rains_data)
  }
  
  # if start_rain is date then set to doy
  # if end_rain is date then set to doy
  
  # calculate season_rain
  return(summary_data)
}
