#' Crop Definitions
#' @description
#' Define crop conditions and create a new crop definition data frame.
#' @param data_name Name of the data frame containing the data.
#' @param year Name of the column representing the year.
#' @param station Name of the column for the station (optional).
#' @param rain Name of the column containing rainfall data.
#' @param day Name of the column containing day data.
#' @param rain_totals Column name for rain totals.
#' @param plant_days Column name for planting days.
#' @param plant_lengths Column name for planting lengths.
#' @param start_check Boolean indicating whether to check start day (default is TRUE).
#' @param season_data_name Name of the season data frame (optional).
#' @param start_day Column name for the start day.
#' @param end_day Column name for the end day.
#' @param return_crops_table Boolean indicating whether to return the full crops table (default is TRUE).
#' @param definition_props Boolean indicating whether to calculate properties (default is TRUE).
#' @param data_book A \code{DataBook} object to use. If \code{NULL}, a new one is created. Default \code{NULL}.
#' @return TODO
#' @export
#'
#' @examples #TODO
crops_definitions <- function (data_name, year, station, rain, day, rain_totals, 
                               plant_days, plant_lengths, start_check = c("both", "yes","no"), 
                               season_data_name, start_day, end_day, return_crops_table = TRUE, 
                               definition_props = TRUE, data_book = NULL){
    if (is.null(data_book)){
      data_book = DataBook$new()
    }
  
    # Running checks
    checkmate::assert_character(data_name)
    checkmate::assert_character(rain)
    checkmate::assert_character(year)
    checkmate::assert_character(station)
    checkmate::assert_character(day)
    checkmate::assert_character(rain_totals)
    checkmate::assert_character(plant_days)
    checkmate::assert_character(plant_lengths)
    checkmate::assert_character(season_data_name)
    checkmate::assert_character(start_day)
    checkmate::assert_character(end_day)
    data_frame <- data_book$get_data_frame(data_name)
    assert_column_names(data_frame, rain)
    assert_column_names(data_frame, year)
    assert_column_names(data_frame, station)
    assert_column_names(data_frame, day)
    assert_column_names(data_frame, rain_totals)
    assert_column_names(data_frame, plant_days)
    assert_column_names(data_frame, plant_lengths)

    assert_column_names(data_frame, start_day)
    assert_column_names(data_frame, end_day)
    
    checkmate::assert_logical(return_crops_table)
    checkmate::assert_logical(definition_props)
  
  
    return(data_book$crops_definitions(data_name = data_name, year = year, station = station, rain = rain, day = day, rain_totals = rain_totals, plant_days = plant_days, plant_lengths = plant_lengths, start_check  = start_check, season_data_name = season_data_name, start_day = start_day, end_day = end_day, return_crops_table  = return_crops_table, definition_props  = definition_props))
         
}