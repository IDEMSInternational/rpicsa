#' Length of the rainfall season
#'
#' Compute the number of days between the start of the rains and the end of the
#' season for each row in a season summary table. Optionally, also derive an
#' \code{occurrence} status from logical start/end status flags. Results are
#' written back into \code{summary_data} stored in \code{data_book}.
#'
#' @param summary_data \code{character(1)} Name of the summary data frame in \code{data_book}
#'   that contains the season boundary columns (e.g., \code{start_rain}, \code{end_rains}).
#' @param start_date \code{character(1)} Column name in \code{summary_data} giving the start
#'   of rains (typically a day-of-year integer).
#' @param end_date \code{character(1)} Column name in \code{summary_data} giving the end
#'   of season (typically a day-of-year integer).
#' @param start_rain_status \code{character(1)} Optional column name in \code{summary_data}
#'   giving a logical flag for whether a valid start of rains was found for that row.
#' @param end_rain_status \code{character(1)} Optional column name in \code{summary_data}
#'   giving a logical flag for whether a valid end of season was found for that row.
#' @param season_length_save_name \code{character(1)} Name of the column to save the computed
#'   season length into. Default \code{"length"}.
#' @param occurrence_save_name \code{character(1)} Name of the column to save the derived
#'   occurrence status into (only used when status columns are supplied). Default \code{"occurrence"}.
#' @param data_book The \code{DataBook} (R6) object holding \code{summary_data}. If \code{NULL},
#'   a new one is created internally (side effects then apply to that object).
#'
#' @details
#' The function always computes the season length as
#' \code{end_date - start_date} and stores it in \code{season_length_save_name}.
#'
#' If both \code{start_rain_status} and \code{end_rain_status} are provided, an
#' additional \code{occurrence} column is created via:
#'
#' \itemize{
#'   \item \code{NA} if either status is \code{NA}
#'   \item \code{"TRUE"} if \code{start_rain_status == end_rain_status == TRUE}
#'   \item \code{"FALSE"} if \code{start_rain_status == end_rain_status == FALSE}
#'   \item \code{"NONE"} if \code{start_rain_status == FALSE} and \code{end_rain_status == TRUE}
#'   \item \code{"MORE"} if \code{start_rain_status == TRUE} and \code{end_rain_status == FALSE}
#' }
#'
#' The \code{occurrence} column is converted to a factor. This function modifies
#' \code{summary_data} inside \code{data_book} and returns invisibly.
#'
#' @export
#' @return Invisibly returns \code{NULL}. Retrieve results with
#'   \code{data_book$get_data_frame(summary_data)}.
#'
#' @seealso \code{\link{start_rains}}, \code{\link{end_rains}}, \code{\link{seasonal_rain}}
#'
#' @examples
#' # Example: compute start/end (DOY + STATUS) then season length for Agades (1946–1950 subset)
#' library(databook)
#' data_book <- DataBook$new()
#' daily_data <- rpicsa::daily_niger |>
#'   dplyr::filter(year > 1945, year <= 1950, station_name == "Agades") |>
#'   dplyr::mutate(year = as.numeric(year))
#' data_book$import_data(list(daily_data = daily_data))
#'
#' start_rains(
#'   data = "daily_data", date_time = "date", station = "station_name",
#'   year = "year", rain = "rain", start_day = 121, end_day = 300,
#'   output = c("doy", "status"), data_book = data_book
#' )
#'
#' end_rains(
#'   data = "daily_data", date_time = "date", station = "station_name",
#'   year = "year", rain = "rain", start_day = 121, end_day = 300,
#'   output = c("doy", "status"), data_book = data_book
#' )
#'
#' seasonal_length(
#'   summary_data        = "daily_data_by_station_name_year",
#'   start_date          = "start_rain",
#'   end_date            = "end_rains",
#'   start_rain_status   = "start_rain_status",
#'   end_rain_status     = "end_rains_status",
#'   data_book           = data_book
#' )
#'
#' # Inspect results
#' data_book$get_data_frame("daily_data_by_station_name_year")
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
  
  # Running checks
  # checks with the summary data frame
  checkmate::assert_string(summary_data)
  checkmate::assert_string(start_date)
  checkmate::assert_string(end_date)
  checkmate::assert_string(start_rain_status, null.ok = TRUE)
  checkmate::assert_string(end_rain_status, null.ok = TRUE)
  checkmate::assert_string(season_length_save_name, null.ok = TRUE)
  checkmate::assert_string(occurrence_save_name, null.ok = TRUE)
  
  data_frame <- data_book$get_data_frame(summary_data)
  assert_column_names(data_frame, start_date)
  assert_column_names(data_frame, end_date)
  if (!is.null(start_rain_status)) assert_column_names(data_frame, start_rain_status)
  if (!is.null(end_rain_status)) assert_column_names(data_frame, end_rain_status)

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