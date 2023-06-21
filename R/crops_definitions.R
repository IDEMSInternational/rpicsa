#' Crop Definitions
#' @description Calculate the probabilities of crop success for given planting maturity lengths, seasonal total rainfall requirements, and planting dates. It is not required that the start of the rains occurs on or before the planting date.
#'
#' @param data The data.frame containing the rainfall data.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param rain \code{character(1)} The name of the rainfall column in \code{data} to apply the function to.
#' @param doy \code{character(1)} The name of the day of year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param rain_totals \code{numeric} Vector containing water requirements.
#' @param plant_days \code{numeric} Vector containing planting dates requirements.
#' @param plant_lengths \code{numeric} Vector containing seasonal crop length requirements.
#' @param start_check \code{logical} A logical value indicating whether to check the start day condition (default is `TRUE`).
#' @param season_data The data frame containing the seasonal data.
#' @param start_day \code{character(1)} The name of the column in the season_data data frame that represents the start day. This can be calculated prior to using this function by the `start_rains` function.
#' @param end_day \code{character(1)} The name of the column in the season_data data frame that represents the end day. This should be calculated prior to using this function by the `end_rains` function.
#'
#' @return TODO
#' @export
#'
#' @examples #TODO
crops_definitions <- function(data, date_time, station = NULL, rain, year = NULL, doy = NULL, rain_totals, plant_days,
                              plant_lengths, start_check = TRUE, season_data = NULL, start_day, end_day) {
  plant_day_name <- "plant_day"
  plant_length_name <- "plant_length"
  rain_total_name <- "rain_total"
  is_station <- !is.null(station)
  checkmate::assert_data_frame(data)
  checkmate::assert_data_frame(season_data)
  cdms.products:::assert_column_names(season_data, start_day)
  cdms.products:::assert_column_names(season_data, end_day)
  checkmate::assert_character(rain)
  checkmate::assert_string(station, null.ok = TRUE)
  checkmate::assert_string(year, null.ok = TRUE)
  checkmate::assert_string(doy, null.ok = TRUE)
  #checkmate::assert_list(rain_totals, types = "numeric")
  #checkmate::assert_list(plant_lengths, types = "numeric")
  #checkmate::assert_list(plant_days, types = "numeric")
  checkmate::assert_logical(start_check, null.ok = TRUE)
  cdms.products:::assert_column_names(data, rain)
  checkmate::assert(checkmate::check_date(data[[date_time]], null.ok = TRUE), 
                    checkmate::check_posixct(data[[date_time]],  null.ok = TRUE))
  
  # calculate doy, year from date_time
  if(is.null(year)){
    year <- "year"
    data[[year]] <- lubridate::year(data[[date_time]])
  }
  if(is.null(doy)){ #(!doy %in% names(data)) {
    doy <- "doy"
    data[[doy]] <- cdms.products::yday_366(data[[date_time]])
  }
  
  if(!is_station) by <- year
  else by <- c(year, station)
  if(is.null(season_data)) season_data <- data
  
  # Create columns for calculations
  expand_list <- list()
  names_list <- c()
  if(is_station) {
    unique_station <- stats::na.omit(unique(data[[station]]))
    expand_list[[length(expand_list) + 1]] <- unique_station
    names_list[length(names_list) + 1] <- station
  }
  expand_list[[length(expand_list) + 1]] <- rain_totals
  names_list[length(names_list) + 1] <- rain_total_name
  expand_list[[length(expand_list) + 1]] <- plant_lengths
  names_list[length(names_list) + 1] <- plant_length_name
  expand_list[[length(expand_list) + 1]] <- plant_days
  names_list[length(names_list) + 1] <- plant_day_name
  expand_list[[length(expand_list) + 1]] <- unique(data[[year]])
  names_list[length(names_list) + 1] <- year
  df <- stats::setNames(expand.grid(expand_list), names_list)
    join_by <- by
    vars <- c(join_by, start_day, end_day)
    season_data <- season_data %>% dplyr::select(dplyr::all_of({{ vars }}))
    df <- dplyr::left_join(df, season_data, by = join_by)
  
  # TODO: just the complete cases for start / end rains.
  df <- df %>% dplyr::filter(stats::complete.cases(df))
  # TODO: what if not yday366 assumption for them?
  if (lubridate::is.Date(df[[start_day]])) df[[start_day]] <- cdms.products::yday_366(df[[start_day]])
  if (lubridate::is.Date(df[[end_day]])) df[[end_day]] <- cdms.products::yday_366(df[[end_day]])
  # Plant day condition
  if(start_check) {
    df$plant_day_cond <- (df[[start_day]] <= df[[plant_day_name]])
  }
  
  # Plant length condition
  df$length_cond <- (df[[plant_day_name]] + df[[plant_length_name]] <= df[[end_day]])
  
  # Rain total condition - calculate the actual rainfall
  df[["rain_total_actual"]] <- sapply(1:nrow(df), 
                                      function(x) {
                                        ind <- data[[year]] == df[[year]][x] & data[[doy]] >= df[[plant_day_name]][x] & 
                                          data[[doy]] < (df[[plant_day_name]][x] + df[[plant_length_name]][x])
                                        if(is_station) ind <- ind & (data[[station]] == df[[station]][x])
                                        rain_values <- data[[rain]][ind]
                                        sum_rain <- sum(rain_values, na.rm = TRUE)
                                        # TODO + 1 is needed because of non leap years
                                        # if period include 29 Feb then period is 1 less than required length
                                        if(length(rain_values) + 1 < df[[plant_length_name]][x] || (anyNA(rain_values) && sum_rain < df[[rain_total_name]][x])) sum_rain <- NA
                                        sum_rain
                                      }
  )
  df$rain_cond <- df[[rain_total_name]] <= df[["rain_total_actual"]]

  # All three conditions met
  df$overall_cond <- ((if(start_check) df$plant_day_cond else TRUE) & df$length_cond & df$rain_cond)

  if (is_station) df <- df %>% dplyr::group_by(.data[[station]])
  df <- df %>% dplyr::group_by(.data[[rain_total_name]], .data[[plant_day_name]], .data[[plant_length_name]], .add = TRUE)
  
  # get definitions
  df <- df %>% dplyr::summarise(prop_success = sum(overall_cond, na.rm = TRUE)/length(stats::na.omit(overall_cond)))
  df$prop_success <- round(df$prop_success, 2)
  return(df)
  }
