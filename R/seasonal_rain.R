#' Seasonal total rainfall
#' @description Total annual rainfall between start of the rains and end of the season.
#' 
#' @inheritParams annual_rain
#' 
#' @param summary_data Summary data frame containing the `start_date` and `end_date` variables. These variables are calculated from start of rains and end of season functions.
#' If `NULL`, `start_date` and `end_date` are calculated from the `start_of_rains` and `end_of_season` functions respectively.
#' @param start_date \code{character(1)} The name of the start of rains column in \code{summary_data}. If \code{NULL} it will be created using the \code{start_of_rains} function.
#' @param end_date \code{character(1)} The name of the end of season column in \code{summary_data}. If \code{NULL} it will be created using the \code{end_of_seasons} function.
#'
#' @return A data.frame with rainfall summaries for each year in the specified season (between start of the rains and end of season).
#' @export
#'
#' @examples # TODO
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
                          eos_start_day = 1, eos_end_day = 366, eos_interval_length = 1, eos_min_rainfall = 10)
{
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
    end_rains_data <- end_rains(data = data, date_time = date_time, station = station, year = year, rain = rain,
                                doy = doy, start_day = eos_start_day, end_day = eos_end_day, output = "doy",
                                interval_length = eos_interval_length, min_rainfall = eos_min_rainfall)
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
