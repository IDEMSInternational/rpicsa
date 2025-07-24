#' End of the rains
#' @description Last occurrence from day q with at least z mm rainfall on a set of days.
#'
#' @param data The data.frame to calculate from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param rain \code{character(1)} The name of the rainfall column in \code{data} to apply the function to.
#' @param doy \code{character(1)} The name of the day of year column in \code{data} to apply the function to. If \code{NULL} it will be created using the \code{date_time} variable.
#' @param s_start_doy \code{numerical(1)} Default `NULL` (if `NULL`, `s_start_doy = 1`. The day of year to state is the first day of year.
#' @param drop \code{logical(1)} default `TRUE`. Whether to drop years where there are `NA` data for the rainfall.
#' @param start_day \code{numerical(1)} The first day to calculate from in the year (1-366).
#' @param end_day \code{numerical(1)} The last day to calculate to in the year (1-366).
#' @param output \code{character(1)} Whether to give the start of rains by day of year (doy), date, and/or status. Default all three selected.
#' @param capacity \code{numerical(1)} Water capacity of the soil (default `60`).
#' @param water_balance_max \code{numerical(1)} Maximum water balance value (default `0.5`).
#' @param evaporation \code{character(1)} Whether to give evaporation as a value or variable. Default `"value"`.
#' @param evaporation_value \code{numerical(1)} If `evaporation = "value"`, the numerical value of amount of evaporation per day (default `5`).
#' @param evaporation_variable \code{character(1)} If `evaporation = "variable"`, the variable in `data` that corresponds to the evaporation column.
#' @param data_book The data book object where the data object is stored, default `NULL`.
#'
#' @return A data.frame with the day of year and/or date for the end of the season for each year (and station).
#' @export

end_season <- function(data, date_time, station = NULL, year = NULL, rain = NULL,
                       doy = NULL,  s_start_doy = NULL, drop = TRUE,
                       start_day = 1, end_day = 366, output = c("doy", "date", "status"),
                       capacity = 60, water_balance_max = 0.5, evaporation = c("value", "variable"),
                       evaporation_value = 5, evaporation_variable = NULL, reducing=FALSE, reducing_value=0.5, data_book = NULL) {
  
    if (is.null(data_book)) {
      data_book <- DataBook$new()
    }
    
    # TODO: set up evaporation_variable
    checkmate::assert_character(data)
    checkmate::assert_character(rain)
    assert_column_names(data, rain)
    checkmate::assert(checkmate::check_date(data[[date_time]], null.ok = TRUE), 
                      checkmate::check_posixct(data[[date_time]],  null.ok = TRUE))
    checkmate::assert_string(station, null.ok = TRUE)
    checkmate::assert_string(year, null.ok = TRUE)
    checkmate::assert_string(doy, null.ok = TRUE)
    checkmate::assert_int(start_day, lower = 1, upper = 365)
    checkmate::assert_int(end_day, lower = 2, upper = 366)
    checkmate::assert_numeric(capacity, lower = 0)
    checkmate::assert_numeric(water_balance_max, lower = 0)
    checkmate::assert_numeric(evaporation_value, lower = 0)
    checkmate::assert_numeric(s_start_doy, lower = 1, upper = 366, null.ok = TRUE)
    #output <- match.arg(output)
    evaporation <- match.arg(evaporation)
    #if (end_day <= start_day) stop("The `end_day` must be after the `start_day`")
    
    # calculate doy, year from date
    if (is.null(year)) {
      data_book$split_date(data_name=data, col_name=date_time, year_val=TRUE, s_start_month=1)
      year <- "year"
    }
    
    if (is.null(doy)){
      data_book$split_date(data_name=data, col_name=date_time, day_in_year_366 =TRUE, s_start_month=1)
      doy <- "doy"
    }
    
    year_type <- data_book$get_column_data_types(data_name=data, columns=year)
    
    data_book$convert_column_to_type(data_name=data, col_names=year, to_type="factor")
    data_book$convert_linked_variable(from_data_frame=data, link_cols=c(year))
    rain_min <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("ifelse(test=is.na(x=", rain, "), yes=0, no=", rain, ")"), result_name="rain_min", calculated_from=setNames(list(rain), data))
    
    if (evaporation == "value"){
      if (!reducing){
        wb_min <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("purrr::accumulate(.f= ~ pmin(pmax(..1 + ..2, 0), ", capacity, "), .x=tail(x=rain_min - ", evaporation_value, ", n=-1), .init=0)"), result_name="wb_min", sub_calculations=list(rain_min))
      }
      else {
        wb_min <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("purrr::accumulate(.f= ~ pmin(pmax(..1 + ..2 - instatClimatic::WB_evaporation(..1, ", reducing_value, ", ", capacity, ", ", evaporation_value, ", ..2), 0), ", capacity, "), .x=tail(x=rain_min, n=-1), .init=0)"), result_name="wb_min", sub_calculations=list(rain_min))
      }
    }
    else if (evaporation == "variable") {
      if (!reducing){
        wb_min <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("purrr::accumulate(.x=tail(rain_min - as.numeric(", evaporation_variable, "), n=-1), .f= ~ pmin(pmax(..1 + ..2, 0), ", capacity, "), .init=0)"), result_name="wb_min", sub_calculations=list(rain_min))
      }
      else {
        wb_min <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("purrr::accumulate2(.x=tail(x=rain_min, n=-1), .y=tail(x=", evaporation_variable, ", n=-1), .f= ~ pmin(pmax(..1 + ..2 - instatClimatic::WB_evaporation(..1, ", reducing_value, ", ", capacity, ", ..3, ..2), 0), ", capacity, "), .init=0)"), result_name="wb_min", sub_calculations=list(rain_min))
      }
    }
    
    rain_max <- instatCalculations::instat_calculation$new(type="calculation", function_exp="ifelse(test=is.na(x=", rain, "), yes=100, no=", rain, ")", result_name="rain_max", calculated_from=setNames(list(rain), data))
    
    if (evaporation == "value"){
      if (!reducing){
        wb_max <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("purrr::accumulate(.f= ~ pmin(pmax(..1 + ..2, 0), ", capacity, "), .x=tail(x=rain_max - ", evaporation_value, ", n=-1), .init=0)"), result_name="wb_max", sub_calculations=list(rain_max))
      }
      else {
        wb_max <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("purrr::accumulate(.f= ~ pmin(pmax(..1 + ..2 - instatClimatic::WB_evaporation(..1, ", reducing_value, ", ", capacity, ", ", evaporation_value, ", ..2), 0), ", capacity, "), .x=tail(x=rain_max, n=-1), .init=0)"), result_name="wb_max", sub_calculations=list(rain_max))
      }
    }
    else if (evaporation == "variable"){
      if (!reducing){
        wb_max <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("purrr::accumulate(.f= ~ pmin(pmax(..1 + ..2, 0), ", capacity, "), .x=tail(x=rain_max - as.numeric(", evaporation_variable, "), n=-1), .init=0)"), result_name="wb_max", sub_calculations=list(rain_max))
      }
      else {
        wb_max <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("purrr::accumulate2(.f= ~ pmin(pmax(..1 + ..2 - instatClimatic::WB_evaporation(..1, ", reducing_value, ", ", capacity, ", ..3, ..2), 0), ", capacity, "), .y=tail(x=", evaporation_variable, ", n=-1), .x=tail(x=rain_max, n=-1), .init=0)"), result_name="wb_max", sub_calculations=list(rain_max))
      }
    }
    
    wb <- instatCalculations::instat_calculation$new(type="calculation", function_exp="ifelse(test=(wb_min != wb_max) | is.na(x=", rain, "), yes=NA, no=wb_min)", result_name="wb", sub_calculations=list(wb_min, wb_max))
    conditions_check <- instatCalculations::instat_calculation$new(type="calculation", function_exp="ifelse((wb <= 0.5) | is.na(x=", rain, "), 1, 0)", result_name="conditions_check", sub_calculations=list(wb))
    conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp="conditions_check == 1", sub_calculations=list(conditions_check))
    grouping_by_station_year <- instatCalculations::instat_calculation$new(type="by", calculated_from=setNames(list(year), data))
    doy_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0(doy, " >= 1 & ", doy, " <= 366"), calculated_from=databook::calc_from_convert(x=setNames(list(doy), data)))
    
    if ("doy" %in% output){
      end_season <- instatCalculations::instat_calculation$new(type="summary", function_exp=paste0("ifelse(test=is.na(x=dplyr::first(x=wb)), yes=NA, no=dplyr::first(x=", doy, "))"), result_name="end_season", calculated_from=setNames(list(doy), data), save=2)
    }
    
    if ("date" %in% output) {
      end_season_date <- instatCalculations::instat_calculation$new(type="summary", function_exp=paste0("dplyr::if_else(condition=is.na(x=dplyr::first(x=wb)), true=as.Date(NA), false=dplyr::first(x=", date_time, "))"), result_name="end_season_date", save=2)
    }
    
    first_sub_calcs <- list()
    if (exists(end_season)) first_sub_calcs <- c(first_sub_calcs, list(end_season))
    if (exists(end_season_date)) first_sub_calcs <- c(first_sub_calcs, list(end_season_date))
    
    end_of_season_combined <- instatCalculations::instat_calculation$new(type="combination", manipulations=list(conditions_filter, grouping_by_station_year, doy_filter), sub_calculations=list(end_season, end_season_date))
    data_book$run_instat_calculation(display=FALSE, param_list=list(drop=FALSE), calc=end_of_season_combined)
    
    if ("status" %in% output){
      conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp="conditions_check == 1 | is.na(conditions_check) | conditions_check == 0", sub_calculations=list(conditions_check))
      end_season_status <- instatCalculations::instat_calculation$new(type="summary", function_exp="ifelse(n() == 0, NA, ifelse(all(conditions_check == 0, na.rm=TRUE), FALSE, NA))", calculated_from=setNames(list(doy), data), result_name="end_season_status", sub_calculations=list(conditions_check), save=2)
      end_of_season_combined_status <- instatCalculations::instat_calculation$new(type="combination", manipulations=list(conditions_filter, grouping_by_station_year, doy_filter), sub_calculations=list(end_season_status))
      data_book$run_instat_calculation(display=FALSE, param_list=list(drop=FALSE), calc=end_of_season_combined_status)
    }
    
    linked_data_name <- data_book$get_linked_to_data_name(data, link_cols=c(year))
    
    if ("status" %in% output){
      calculated_from_list <- c(setNames("end_season_status", linked_data_name), setNames("end_season", linked_data_name))
      end_season_status_2 <- instatCalculations::instat_calculation$new(type="calculation", function_exp="ifelse(!is.na(end_season), TRUE, end_season_status)", calculated_from=calculated_from_list, result_name="end_season_status", save=2)
      end_season_combined_status_2 <- instatCalculations::instat_calculation$new(type="combination", sub_calculations=list(end_season_status_2))
      data_book$run_instat_calculation(display=FALSE, param_list=list(drop=FALSE), calc=end_season_combined_status_2)
    }
    
    data_book$convert_column_to_type(data_name=data, col_names=year, to_type=year_type)
    data_book$convert_column_to_type(data_name=linked_data_name, col_names=year, to_type=year_type)
  
}
