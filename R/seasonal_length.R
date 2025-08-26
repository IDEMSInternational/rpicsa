
#' Length of the season
#' @description Number of days between start of the rains and end of the season
#'
# @inheritParams seasonal_rain
#' @param summary_data Summary data frame containing the `start_date` and `end_date` variables. These variables are calculated from start of rains and end of season functions.
#' If `NULL`, `start_date` and `end_date` are calculated from the `start_of_rains` and `end_of_season` functions respectively.
#' @param start_date \code{character(1)} The name of the start of rains column in \code{summary_data}. If \code{NULL} it will be created using the \code{start_of_rains} function.
#' @param end_date \code{character(1)} The name of the end of season column in \code{summary_data}. If \code{NULL} it will be created using the \code{end_of_seasons} function.
#' @param s_start_month \code{integer(1)} Month (1–12) to treat as the start of the “year” when creating \code{year} or \code{doy}. Default \code{NULL} (assumes January).
#' @param data The daily data.frame to calculate rainfall from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param rain \code{character(1)} The name of the rainfall column in \code{data} to apply the function to.
#' @param doy \code{character(1)} The name of the day of year column in \code{data} to apply the function to. If \code{NULL} it will be created using the \code{date_time} variable.
#' @param threshold \code{numerical(1)} threshold value for amount (mm) of rainfall in order to count it as a rainy day.
#' @param sor_start_day \code{numerical(1)} The first day to calculate from in the year (1-366).
#' @param sor_end_day \code{numerical(1)} The last day to calculate to in the year (1-366).
#' @param sor_total_rainfall_comparison Method for multi‐day definition: \code{"amount"}, \code{"proportion"}, or \code{"evaporation"}. Default \code{"amount"}.
#' @param sor_over_days \code{numerical(1)} Only works if `total_rainfall = TRUE`. This is the number of days to total the rainfall over.
#' @param sor_amount_rain \code{numerical(1)} If `sor_total_rainfall_comparison = "amount"`, the amount of rainfall to expect over the period defined in `over_days`. 
#' @param sor_prob_rain_day \code{numerical(1)} Only works if `sor_total_rainfall_comparison = "proportion"` This is the proportion.
#' @param sor_evaporation_variable \code{character(1)} Only works if `sor_total_rainfall_comparison = "evaporation"`. Name of evaporation (mm) column, if using \code{"evaporation"}. Default \code{NULL}.
#' @param sor_fraction \code{numeric(1)} Multiplier (0–1) applied to evaporation values when \code{total_rainfall_comparison="evaporation"}. Default \code{0.5}.
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
#' @param start_rain_status \code{character(1)} The name of the variable that indicates the start of rain status
#' @param start_rain_status \code{character(1)} The name of the variable that indicates the end of rain status
#' @param season_length_save_name \code{character(1)} The name used to save the length of season
#' @param occurrence_save_name \code{character(1)} The name used to save the status results
#' @param data_book The data book object where the data object is stored, default `NULL`.
#' @return A data.frame with length of rainfall season for each year in the specified season (between start of the rains and end of season).
#' @export 
#'
#' @examples
#' # Example of season
#' library(databook)
#' data_book <- DataBook$new()
#' new_RDS <- readRDS(file="C:/Users/HP/Downloads/EPICSA_Example.RDS")
#' data_book$import_RDS(data_RDS=new_RDS)
#' my_data <- data_book$get_data_frame("ghana_by_station_year")
#' rm(new_RDS)
#' seasonal_length(start_date = "start_rain", 
#'                 end_date = "end_rains", 
#'                 s_start_month = 1,
#'                 data = "my_data", 
#'                 data_book = NULL, 
#'                 start_rain_status = "start_rain_status", 
#'                 end_rain_status = "end_rains_status")

seasonal_length <- function(summary_data = NULL, start_date = NULL, end_date = NULL, s_start_month = 1,
                            data = NULL, date_time = NULL, rain = NULL, year = NULL, station = NULL, doy = NULL, 
                            # start of rains parameters
                            threshold = 0.85, sor_start_day = 1, sor_end_day = 366,
                            sor_total_rainfall_comparison = c("amount", "proportion", "evaporation"), sor_over_days = 1, sor_amount_rain = 20, sor_prob_rain_day = 0.8,
                            sor_evaporation_variable = NULL, sor_fraction = 0.5,
                            sor_number_rain_days = FALSE, sor_min_rain_days = 1, sor_rain_day_interval = 2,
                            sor_dry_spell = FALSE, sor_spell_interval = 21, sor_spell_max_dry_days = 9,
                            sor_dry_period = FALSE, sor_period_interval = 45, sor_max_rain = 40, sor_period_max_dry_days = 30,
                            # end of rains parameters
                            end_type = c("season", "rains"),
                            eos_start_day = 1, eos_end_day = 366,
                            eor_interval_length = 1, eor_min_rainfall = 10,
                            eos_capacity = 60, eos_water_balance_max = 0.5, eos_evaporation = c("value", "variable"),
                            eos_evaporation_value = 5, eos_evaporation_variable = NULL, data_book, season_length_save_name="length", 
                            occurrence_save_name="length_status", start_rain_status = NULL, end_rain_status = NULL)
{
  end_type <- match.arg(end_type)
  
  # creating the a new databook object if it doesn't exist
  if (is.null(data_book)){
    data_book = DataBook$new()
  }
  
  # from "start" and "end" variables columns (this should always run):
  length_of_season <- instatCalculations::instat_calculation$new(
    type="calculation", 
    function_exp=paste0(end_date, " - ", start_date), 
    result_name=season_length_save_name, 
    calculated_from=setNames(list(start_date, end_date), c(summary_data, summary_data)), 
    save=2)
  
  # if the "status" variables are given (this should be optional):
  if (!is.null(start_rain_status) | !is.null(end_rain_status)){
    start_end_status <- instatCalculations::instat_calculation$new(
      type="calculation", 
      function_exp=paste0("dplyr::case_when(is.na(", start_rain_status, ") | is.na(", end_rain_status, ") ~ NA_character_, ", start_rain_status, " == ", end_rain_status, " ~ as.character(", start_rain_status, "), ", start_rain_status, " == FALSE & ", end_rain_status, " == TRUE ~ 'NONE', ", start_rain_status, " == TRUE & ", end_rain_status, " == FALSE ~ 'MORE')"), 
      result_name=occurrence_save_name, 
      calculated_from=setNames(list(start_rain_status, end_rain_status), c(data, data)), 
      save=2)
  }
  
  
  # if "status" variables are given, we need to run a combination as we are giving two calculations:
  if (!is.null(start_rain_status) | !is.null(end_rain_status)){
    length_rains_combined <- instatCalculations::instat_calculation$new(
      type="combination", 
      sub_calculation=list(length_of_season, start_end_status))
    data_book$run_instat_calculation(calc=length_rains_combined, display=FALSE)
  } 
  # if "status" variables are not given, we only need to run the one calculation of length_of_season:
  else {
    data_book$run_instat_calculation(calc=length_of_season, display=FALSE)
  }
  
  data_book$convert_column_to_type(data_name=data, col_names=occurrence_save_name, to_type="factor")
}
