#' Crop Definitions
#' @description Calculate the probabilities of crop success for given planting maturity lengths, seasonal total rainfall requirements, and planting dates. It is not required that the start of the rains occurs on or before the planting date.
#'
#' @param data The data.frame containing the rainfall data.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param rain \code{character(1)} The name of the rainfall column in \code{data} to apply the function to.
#' @param doy \code{character(1)} The name of the day of year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param water_requirements \code{numeric} Vector containing water requirements.
#' @param planting_dates \code{numeric} Vector containing planting dates requirements.
#' @param planting_length \code{numeric} Vector containing seasonal crop length requirements.
#' @param start_check \code{logical} A logical value indicating whether to check the start day condition (default is `TRUE`).
#' @param season_data The data frame containing the seasonal data.
#' @param start_day \code{character(1)} The name of the column in the season_data data frame that represents the start day. This can be calculated prior to using this function by the `start_rains` function.
#' @param end_day \code{character(1)} The name of the column in the season_data data frame that represents the end day. This should be calculated prior to using this function by the `end_rains` function.
#' @param seasonal_length \code{character(1)} If `data` is `NULL` then you can read in the seasonal length amount from the `season_data` data frame.
#' @param seasonal_rain \code{character(1)} If `data` is `NULL` then you can read in the seasonal rainfall amount from the `season_data` data frame.
#'
#' @return TODO
#' @export
#'
#' @examples #TODO
crops_definitions <- function(data = NULL, date_time = NULL, station = NULL, rain = NULL, year = NULL, 
                              doy = NULL, water_requirements, planting_dates, planting_length, start_check = TRUE, 
                              season_data = NULL, start_day, end_day = NULL, seasonal_length = NULL,
                              seasonal_rain = NULL) {
  planting_day_name <- "planting_day"
  planting_length_name <- "planting_length"
  water_requirements_name <- "water_requirements"
  
  if (is.null(data)){
    # if the data is NULL, then we just need three columns from the seson_data - start_day, seasonal_length, seasonal_rain
    if (is.null(start_day)){ stop("If the data is NULL then start_day must be given")}
    if (is.null(seasonal_length)){ stop("If the data is NULL then seasonal_length must be given")}
    if (is.null(seasonal_rain)){ stop("If the data is NULL then seasonal_rain must be given")}
    
    expand_list <- list()
    names_list <- c()
    expand_list[[length(expand_list) + 1]] <- water_requirements
    names_list[length(names_list) + 1] <- water_requirements_name
    expand_list[[length(expand_list) + 1]] <- planting_length
    names_list[length(names_list) + 1] <- planting_length_name
    expand_list[[length(expand_list) + 1]] <- planting_dates
    names_list[length(names_list) + 1] <- planting_day_name
    criteria_df <- stats::setNames(expand.grid(expand_list), names_list)
    
    # Function to calculate the success percentage
    calculate_success_percentage <- function(season_data, criteria_df, start_check) {
      is_station <- "station" %in% names(season_data)
      
      result_df <- data.frame()
      
      for (i in 1:nrow(criteria_df)) {
        water_req <- criteria_df$water_requirements[i]
        plant_len <- criteria_df$planting_length[i]
        plant_day <- criteria_df$planting_day[i]
        
        station_results <- season_data
        
        if (start_check) {
          station_results <- station_results %>%
            dplyr::mutate(planting_day_cond = station_results[[start_day]] <= plant_day)
        }
        station_results <- station_results %>%
          dplyr::mutate(length_cond = (plant_len) <= seasonal_length) %>%
          dplyr::mutate(rain_cond = (water_req) <= seasonal_rain) %>%
          dplyr::mutate(overall_cond = ((if (start_check) planting_day_cond else TRUE) & length_cond & rain_cond))
        
        if (is_station) station_results <- station_results %>% dplyr::group_by(station)
        
        station_results <- station_results %>%
          dplyr::summarise(prop_success = sum(overall_cond, na.rm = TRUE) / dplyr::n()) %>%
          dplyr::mutate(rain_total = water_req, plant_day = plant_day, plant_length = plant_len)
        
        result_df <- dplyr::bind_rows(result_df, station_results)
      }
      if (is_station){
        result_df <- result_df %>% dplyr::select(c(station, rain_total, plant_day, plant_length, prop_success = prop_success))
      } else {
        result_df <- result_df %>% dplyr::select(c(rain_total = water_requirements, plant_day = planting_day, plant_length = planting_length, prop_success = prop_success))
      }
      return(result_df)
    }
    
    # Execute the function
    result_df <- calculate_success_percentage(season_data, criteria_df, start_check = start_check)
    
    # Display the result
    return(result_df)
  } else {
    is_station <- !is.null(station)
    checkmate::assert_data_frame(data)
    checkmate::assert_data_frame(season_data)
    assert_column_names(season_data, start_day)
    assert_column_names(season_data, end_day)
    checkmate::assert_character(rain)
    checkmate::assert_string(station, null.ok = TRUE)
    checkmate::assert_string(year, null.ok = TRUE)
    checkmate::assert_string(doy, null.ok = TRUE)
    checkmate::assert_logical(start_check, null.ok = TRUE)
    assert_column_names(data, rain)
    checkmate::assert(checkmate::check_date(data[[date_time]], 
                                            null.ok = TRUE), checkmate::check_posixct(data[[date_time]], 
                                                                                      null.ok = TRUE))
    if (is.null(year)) {
      year <- "year"
      data[[year]] <- lubridate::year(data[[date_time]])
    }
    if (is.null(doy)) {
      doy <- "doy"
      data[[doy]] <- yday_366(data[[date_time]])
    }
    if (is.null(season_data))
      season_data <- data
    expand_list <- list()
    names_list <- c()
    if (is_station) {
      unique_station <- stats::na.omit(unique(data[[station]]))
      expand_list[[length(expand_list) + 1]] <- unique_station
      names_list[length(names_list) + 1] <- station
    }
    expand_list[[length(expand_list) + 1]] <- water_requirements
    names_list[length(names_list) + 1] <- water_requirements_name
    expand_list[[length(expand_list) + 1]] <- planting_length
    names_list[length(names_list) + 1] <- planting_length_name
    expand_list[[length(expand_list) + 1]] <- planting_dates
    names_list[length(names_list) + 1] <- planting_day_name
    expand_list[[length(expand_list) + 1]] <- unique(data[[year]])
    names_list[length(names_list) + 1] <- year
    df <- stats::setNames(expand.grid(expand_list), names_list)
    if (!is_station) {
      season_data <- season_data %>% dplyr::select(c(.data[[year]], .data[[station]], start_day, end_day))
    } else {
      season_data <- season_data %>% dplyr::select(c(.data[[year]], start_day, end_day))
    }
    season_data <- stats::na.omit(season_data)
    df <- dplyr::full_join(df, season_data)
    #df <- df %>% dplyr::filter(stats::complete.cases(df))
    if (lubridate::is.Date(df[[start_day]])) 
      df[[start_day]] <- yday_366(df[[start_day]])
    if (lubridate::is.Date(df[[end_day]])) 
      df[[end_day]] <- yday_366(df[[end_day]])
    if (start_check) {
      df$planting_day_cond <- (df[[start_day]] <= df[[planting_day_name]])
    }
    df$length_cond <- (df[[planting_day_name]] + df[[planting_length_name]] <= 
                         df[[end_day]])
    df[["water_requirements_actual"]] <- sapply(1:nrow(df), function(x) {
      ind <- data[[year]] == df[[year]][x] & data[[doy]] >= 
        df[[planting_day_name]][x] & data[[doy]] < (df[[planting_day_name]][x] + 
                                                      df[[planting_length_name]][x])
      if (is_station) 
        ind <- ind & (data[[station]] == df[[station]][x])
      rain_values <- data[[rain]][ind]
      sum_rain <- sum(rain_values, na.rm = TRUE)
      if (length(rain_values) + 1 < df[[planting_length_name]][x] || 
          (anyNA(rain_values) && sum_rain < df[[water_requirements_name]][x])) 
        sum_rain <- NA
      sum_rain
    })
    df$rain_cond <- df[[water_requirements_name]] <= df[["water_requirements_actual"]]
    df$overall_cond <- ((if (start_check) 
      df$planting_day_cond
      else TRUE) & df$length_cond & df$rain_cond)
    if (is_station) 
      df <- df %>% dplyr::group_by(.data[[station]])
    df <- df %>% dplyr::group_by(.data[[water_requirements_name]], .data[[planting_day_name]], 
                                 .data[[planting_length_name]], .add = TRUE)
    df <- df %>% dplyr::summarise(prop_success = sum(overall_cond, 
                                                     na.rm = TRUE)/length(stats::na.omit(overall_cond)))
    df$prop_success <- round(df$prop_success, 2)
    df <- df %>% dplyr::select(c(station, rain_total = water_requirements, plant_day = planting_day, plant_length = planting_length, prop_success = prop_success))
    return(df)
  }
}