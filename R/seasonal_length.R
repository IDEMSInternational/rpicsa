#' Length of the season
#' @description Number of days between start of the rains and end of the season
#'
# @inheritParams seasonal_rain
#' @param summary_data Summary data frame containing the `start_date` and `end_date` variables. These variables are calculated from start of rains and end of season functions.
#' @param start_date \code{character(1)} The name of the start of rains column in \code{summary_data}. If \code{NULL} it will be created using the \code{start_of_rains} function.
#' @param end_date \code{character(1)} The name of the end of season column in \code{summary_data}. If \code{NULL} it will be created using the \code{end_of_seasons} function.
#' @param start_rain_status \code{character(1)} The name of the variable that indicates the start of rain status
#' @param end_rain_status \code{character(1)} The name of the variable that indicates the end of rain status
#' @param season_length_save_name \code{character(1)} The name used to save the length of season
#' @param occurrence_save_name \code{character(1)} The name used to save the status results
#' @param data_book The data book object where the data object is stored, default `NULL`.
#' @return A data.frame with length of rainfall season for each year in the specified season (between start of the rains and end of season).
#' @export 
#'
#' @examples
#' # Example: First calculate start and end rains
#' data_book <- DataBook$new()
#' daily_data <- rpicsa::daily_niger %>%
#'   dplyr::filter(year <= 1950) %>%
#'   dplyr::filter(year > 1945) %>%
#'   dplyr::mutate(year = as.numeric(year)) %>%
#'   dplyr::filter(station_name == "Agades")
#' data_book$import_data(list(daily_data = daily_data))
#' 
#' start_rains(data = "daily_data",
#'             date_time = "date",
#'             station = "station_name",
#'             year = "year",
#'             rain = "rain",
#'             start_day = 121,
#'             end_day = 300,
#'             output = c("doy", "status"),
#'             data_book = data_book)
#' 
#' end_rains(data = "daily_data",
#'           date_time = "date",
#'           station = "station_name",
#'           year = "year",
#'           rain = "rain",
#'           start_day = 121,
#'           end_day = 300,
#'           output = c("doy", "status"),
#'           data_book = data_book)
#' 
#' # then run the summaries for seasonal rainfall
#' seasonal_length(summary_data = "daily_data_by_station_name_year",
#'                 start_date = "start_rain",
#'                 end_date = "end_rains",
#'                 start_rain_status = "start_rain_status",
#'                 end_rain_status = "end_rains_status",
#'                 data_book = data_book
#' )
#' daily_data_by_station_name_year <- data_book$get_data_frame("daily_data_by_station_name_year")
#' daily_data_by_station_name_year 
seasonal_length <- function(summary_data, start_date, end_date,
                            start_rain_status = NULL, end_rain_status = NULL,
                            season_length_save_name = "length", 
                            occurrence_save_name = "occurrence",
                            data_book = NULL)
{
  # creating the a new databook object if it doesn't exist
  if (is.null(data_book)){
    data_book = DataBook$new()
  }
  
  # from "start" and "end" variables columns (this should always run):
  length_of_season <- instatCalculations::instat_calculation$new(
    type="calculation", 
    function_exp = paste0(end_date, " - ", start_date), 
    result_name = season_length_save_name, 
    calculated_from = setNames(list(start_date, end_date), c(summary_data, summary_data)), 
    save=2)
  
  # if the "status" variables are given (this should be optional):
  if (!is.null(start_rain_status) | !is.null(end_rain_status)){
    start_end_status <- instatCalculations::instat_calculation$new(
      type="calculation", 
      function_exp=paste0(
        "dplyr::case_when(is.na(", start_rain_status, ") | is.na(", end_rain_status, ") ~ NA_character_, ",
        start_rain_status, " == ", end_rain_status, " ~ as.character(", start_rain_status, "), ",
        start_rain_status, " == FALSE & ", end_rain_status, " == TRUE ~ 'NONE', ",
        start_rain_status, " == TRUE & ", end_rain_status, " == FALSE ~ 'MORE')"), 
      result_name = occurrence_save_name, 
      calculated_from = setNames(list(start_rain_status, end_rain_status), c(summary_data, summary_data)), 
      save=2)
  }
  
  # if "status" variables are given, we need to run a combination as we are giving two calculations:
  if (!is.null(start_rain_status) | !is.null(end_rain_status)){
    length_rains_combined <- instatCalculations::instat_calculation$new(
      type = "combination", 
      sub_calculation = list(length_of_season, start_end_status))
    data_book$run_instat_calculation(calc=length_rains_combined, display=FALSE)
    data_book$convert_column_to_type(data_name = summary_data, col_names = occurrence_save_name, to_type="factor")
  } else {
    # if "status" variables are not given, we only need to run the one calculation of length_of_season:
    data_book$run_instat_calculation(calc=length_of_season, display=FALSE)
  }
}