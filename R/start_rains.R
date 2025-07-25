#' Start of the rains
#' @description Used to present or summarise daily rainfall data in a way that corresponds to the start of the rainy season for many definitions.
#'
#' @param data The data.frame to calculate from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param rain \code{character(1)} The name of the rainfall column in \code{data} to apply the function to.
#' @param threshold \code{numerical(1)} threshold value for amount (mm) of rainfall in order to count it as a rainy day.
#' @param doy \code{character(1)} The name of the day of year column in \code{data} to apply the function to. If \code{NULL} it will be created using the \code{date_time} variable.
#' @param start_day \code{numerical(1)} The first day to calculate from in the year (1-366).
#' @param end_day \code{numerical(1)} The last day to calculate to in the year (1-366).
#' @param s_start_doy \code{numerical(1)} Default `NULL` (if `NULL`, `s_start_doy = 1`. The day of year to state is the first day of year.
#' @param drop \code{logical(1)} default `TRUE`. Whether to drop years where there are `NA` data for the rainfall.
#' @param output \code{character(1)} Whether to give the start of rains by day of year (doy), date, and/or status. Default all three selected.
#' @param include_status \code{logical(1)} Default `FALSE`. If `drop = TRUE`, this status indicates whether the `NA` is due to no start date for that year, or due to a lack of data available. If `start_rain_status` is `TRUE` in the data, then that indicates the `NA` is due to a lack of data; if it is `FALSE` indicates the `NA` is due to no start rain fitting the definition for that year.
#' @param total_rainfall \code{logical(1)} default `TRUE`. Start of the rains to be defined by the total or proportion of rainfall over a period.
#' @param over_days \code{numerical(1)} Only works if `total_rainfall = TRUE`. This is the number of days to total the rainfall over.
#' @param amount_rain \code{numerical(1)} If `total_rainfall = TRUE` and `proportion = FALSE`, the amount of rainfall to expect over the period defined in `over_days`. 
#' @param proportion \code{logical(1)} default `FALSE`, only valid if `total_rainfall = TRUE`. If `TRUE`, Start of the rains to be defined by proportion of rainfall over a period. This proportion is given in `prob_rain_day`. Otherwise, defined by the amount of rainfall over a period. The amount is given in `amount_rain`.
#' @param prob_rain_day \code{numerical(1)} Only works if `total_rainfall = TRUE` and `proportion = TRUE` This is the number 
#' @param number_rain_days \code{logical(1)} default `FALSE`. If `TRUE`, define start of the rains by the number of rainy days (`min_rain_days`) over a period. The period is given in days in the `rain_day_interval` parameter.
#' @param min_rain_days \code{numerical(1)} Only if `number_rain_days = TRUE`. This is the minimum number of rainy days to define start of rains in a given period. The period is given in days in the `rain_day_interval` parameter.
#' @param rain_day_interval \code{numerical(1)} Only if `number_rain_days = TRUE`, the interval in days that the `number_rain_days` is defined in.
#' @param dry_spell \code{logical(1)} default `FALSE`. If `TRUE`, define start of the rains by a maximum number of dry days (`spell_max_dry_days`) over a given period of days (`spell_interval`).
#' @param spell_max_dry_days \code{numerical(1)} Only if `dry_spell = TRUE`. This is the maximum number of dry days to define start of rains in a given period. The period is given in days in the `spell_interval` parameter.
#' @param spell_interval \code{numerical(1)} Only if `dry_spell = TRUE`, the interval in days that the `dry_spell` is defined in.
#' @param dry_period \code{logical(1)} default `FALSE`. If `TRUE`, define start of the rains by the maximum rain and maximum dry days in a given interval. The maximum rainfall amount is given in the `max_rain` parameter, the maximum dry days is given in the `period_max_dry_days` parameter, and the interval length is given in the `period_interval` parameter. 
#' @param max_rain \code{numerical(1)} Only if `dry_period = TRUE`, the maximum rainfall to occur in a given period.
#' @param period_max_dry_days \code{numerical(1)} Only if `dry_period = TRUE`. the maximum period of dry days to occur in a given period.
#' @param period_interval \code{numerical(1)} Only if `dry_period = TRUE`, the interval in days that the `dry_period` is defined in.
#' @param data_book The data book object where the data object is stored, default `NULL`.
#' @param evaporation \code{Logical(1)} If `TRUE`, the function will include evaporation data in the calculation of the wet spell threshold. This allows comparing rainfall against evaporative demand to determine the start of rains more accurately.
#' @param evaporation_variable \code{Character(1)} The name of the variable (column) in the dataset representing evaporation values. Required if `evaporation = TRUE`. This variable should be numeric and represent daily evaporation measurements.
#' @param fraction \code{Numeric(1)} A multiplier applied to the evaporation variable to represent the proportion of evaporation considered significant in rainfall evaluation. Defaults to 0.5. Only used if `evaporation = TRUE`.
#' 
#' 
#' @return A data.frame with the date and/or day of year for the start of the rains for each year
#' @export
#'
#' @examples #TODO#
#' # check against R-Instat function
start_rains <- function(data, date_time, station = NULL, year = NULL, rain = NULL, threshold = 0.85,
                        doy = NULL, start_day = 1, end_day = 366, s_start_doy = NULL,
                        drop = TRUE,
                        output = c("doy", "date", "status"),
                        include_status = FALSE,
                        total_rainfall = TRUE, over_days = 1, amount_rain = 20, proportion = FALSE, prob_rain_day = 0.8,
                        number_rain_days = FALSE, min_rain_days = 1, rain_day_interval = 2,
                        dry_spell = FALSE, spell_interval = 21, spell_max_dry_days = 9,
                        dry_period = FALSE, period_interval = 45, max_rain = 40, period_max_dry_days = 30, data_book = NULL,
                        evaporation = FALSE, evaporation_variable = NULL, fraction = 0.5) {
  
    # creating the a new databook object if it doesn't exist
    if (is.null(data_book)){
      data_book = DataBook$new()
    }
    
    
    checkmate::assert_character(data)
    checkmate::assert_character(rain)
    data_frame <- data_book$get_data_frame(data)
    assert_column_names(data_frame, rain)
    #rpicsa:::assert_column_names(data, rain)
    checkmate::assert(checkmate::check_date(data_frame[[date_time]], null.ok = TRUE), 
                      checkmate::check_posixct(data_frame[[date_time]],  null.ok = TRUE))
    checkmate::assert_string(station, null.ok = TRUE)
    checkmate::assert_string(year, null.ok = TRUE)
    #checkmate::assert_string(doy, null.ok = TRUE)
    # if (!is.null(station)) assert_column_names(data, station)
    # if (!is.null(date_time)) assert_column_names(data, date_time)
    # if (!is.null(year)) assert_column_names(data, year)
    checkmate::assert_numeric(s_start_doy, lower = 1, upper = 366, null.ok = TRUE)
    checkmate::assert_logical(total_rainfall, null.ok = TRUE)
    checkmate::assert_logical(proportion, null.ok = TRUE)
    checkmate::assert_logical(number_rain_days, null.ok = TRUE)
    checkmate::assert_logical(dry_spell, null.ok = TRUE)
    checkmate::assert_logical(dry_period, null.ok = TRUE)
    checkmate::assert_number(prob_rain_day, lower = 0, upper = 1, null.ok = TRUE)
    checkmate::assert_int(start_day, lower = 1, upper = 365, null.ok = TRUE)
    checkmate::assert_int(end_day, lower = 2, upper = 366, null.ok = TRUE)
    checkmate::assert_int(over_days, lower = 1, null.ok = TRUE)
    checkmate::assert_int(amount_rain, lower = 0, null.ok = TRUE)
    checkmate::assert_int(min_rain_days, lower = 0, null.ok = TRUE)
    checkmate::assert_int(rain_day_interval, lower = 1, null.ok = TRUE)
    checkmate::assert_int(spell_interval, lower = 1, null.ok = TRUE)
    checkmate::assert_int(spell_max_dry_days, lower = 0, null.ok = TRUE)
    checkmate::assert_int(period_interval, lower = 1, null.ok = TRUE)
    checkmate::assert_int(max_rain, lower = 0, null.ok = TRUE)
    checkmate::assert_int(period_max_dry_days, lower = 0, null.ok = TRUE)
    
    if (end_day <= start_day) stop("The `end_day` must be after the `start_day`")
    if (number_rain_days){
      if (rain_day_interval < min_rain_days) stop("Value given in `rain_day_interval` must be equal to or greater than the value given in `min_rain_days`")
    }
    if (dry_spell){
      if (spell_interval < spell_max_dry_days) stop("Value given in `spell_interval` must be equal to or greater than the value given in `spell_max_dry_days`")
    }
    if (dry_period){
      if (period_interval < period_max_dry_days) stop("Value given in `period_interval` must be equal to or greater than the value given in `period_max_dry_days`")
    } 
    # output <- match.arg(output)
    
    # calculate doy, year from date
    if(is.null(year)){#if(!year %in% names(data)) { # do instead of is.null because of epicsawrap. we always read in "year" whether it exists or not.
      data_book$split_date(data_name = data, col_name = date_time, year_val = TRUE, s_start_month = 1)
      year <- "year"
    }
    if(is.null(doy)){ 
      data_book$split_date(data_name = data, col_name = date_time, day_in_year_366 =TRUE, s_start_month = 1)
      doy <- "doy"
    }
    
    year_type <- data_book$get_column_data_types(data_name=data, columns=year)
    data_book$convert_column_to_type(data_name=data, col_names=year, to_type="factor")
    if (station) {
      station_type <- data_book$get_column_data_types(data_name=data, columns=station)
      data_book$convert_column_to_type(data_name = data, col_names = station, to_type="factor")
      data_book$convert_linked_variable(from_data_frame=data, link_cols=c(year, station))
      grouping_by_station <- instatCalculations::instat_calculation$new(type="by", calculated_from=setNames(list(station), data))
    }
    data_book$convert_linked_variable(from_data_frame=data, link_cols=c(year))
    
    if (evaporation) {
      fraction_evap <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0(evaporation_variable, " * ", fraction), result_name="fraction_evap", calculated_from=setNames(list(evaporation_variable), data))
      roll_sum_evap <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("RcppRoll::roll_sumr(x=fraction_evap, n=", over_days, ", fill=NA, na.rm=FALSE)"), result_name="roll_sum_evap", sub_calculations=list(fraction_evap))
    }
    
    if (station){
      roll_sum_rain <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("RcppRoll::roll_sumr(x=", rain, ", n=", over_days, ", fill=NA, na.rm=FALSE)"), result_name="roll_sum_rain", calculated_from=setNames(list(rain), data), manipulations=list(grouping_by_station))
    }
    else {
      roll_sum_rain <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("RcppRoll::roll_sumr(x=", rain, ", n=", over_days, ", fill=NA, na.rm=FALSE)"), result_name="roll_sum_rain", calculated_from=setNames(list(rain), data))
      
    }
    
    rain_day <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0(rain, " >= ", threshold), result_name="rain_day", calculated_from=setNames(list(rain), data))
    if (total_rainfall && proportion){
      total_rainfall_wet_spell <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("quantile(x=roll_sum_rain, probs=", prob_rain_day, ", na.rm=TRUE)"), result_name="wet_spell", sub_calculations=list(roll_sum_rain))
    }
    
    # If either one of number_rain_days, dry_spells or dry_period is TRUE, set the appropriate conditions_filter function and the selected options for output
    if (number_rain_days){
      roll_n_rain_days <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("RcppRoll::roll_sumr(x=rain_day, n=", rain_day_interval, ", fill=NA, na.rm=FALSE)"), result_name="roll_n_rain_days", sub_calculations=list(rain_day))
      # setting conditions filter under this setting
      if (total_rainfall && proportion){
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > wet_spell & roll_n_rain_days >= ", min_rain_days, ") | is.na(x=", rain, ") | is.na(x=roll_sum_rain) | is.na(x=roll_n_rain_days)"), sub_calculations=list(roll_sum_rain, roll_n_rain_days, total_rainfall_wet_spell))
      }
      else if (evaporation){
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > roll_sum_evap & roll_n_rain_days >= ", min_rain_days, ") | is.na(x=", rain, ") | is.na(x=roll_sum_rain) | is.na(x=roll_n_rain_days)"), sub_calculations=list(roll_sum_rain, roll_sum_evap, roll_n_rain_days))
      }
      else {
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > 20 & roll_n_rain_days >= ", min_rain_days, ") | is.na(x=", rain, ") | is.na(x=roll_sum_rain) | is.na(x=roll_n_rain_days)"), sub_calculations=list(roll_sum_rain, roll_n_rain_days))
      }
      
      # setting output options under this setting
      if ("doy" %in% output){
        start_of_rains_doy <- instatCalculations::instat_calculation$new(type="summary", function_exp=paste0("ifelse(test=is.na(x=dplyr::first(x=", rain, ")) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_n_rain_days)), yes=NA, no=dplyr::first(x=", doy, ", default=NA))"), result_name="start_rain", save=2)
      }
      if ("date" %in% output){
        start_rain_date <- instatCalculations::instat_calculation$new(type="summary", function_exp=paste0("dplyr::if_else(condition=is.na(x=dplyr::first(x=", rain, ")) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_n_rain_days)), true=as.Date(NA), false=dplyr::first(", date, ", default=NA))"), result_name="start_rain_date", save=2)
      }
    }
    if (dry_spell){
      dry_spell <- instatCalculations::instat_calculation$new(type="calculation", function_exp="instatClimatic::spells(x=rain_day == 0)", result_name="dry_spell", sub_calculations=list(rain_day))
      roll_max_dry_spell <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("dplyr::lead(x=RcppRoll::roll_maxl(n=", spell_interval, ", x=dry_spell, fill=NA))"), result_name="roll_max_dry_spell", sub_calculations=list(dry_spell))
      # setting conditions filter under this setting
      if (total_rainfall && proportion){
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > wet_spell & roll_max_dry_spell <= ", spell_max_dry_days, ") | is.na(x=", rain, ") | is.na(x=roll_sum_rain) | is.na(x=roll_max_dry_spell)"), sub_calculations=list(roll_sum_rain, roll_max_dry_spell, total_rainfall_wet_spell))
      }
      else if (evaporation) {
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > roll_sum_evap & roll_max_dry_spell <= ", spell_max_dry_days, ") | is.na(x=", rain, ") | is.na(x=roll_sum_rain) | is.na(x=roll_max_dry_spell)"), sub_calculations=list(roll_sum_rain, roll_sum_evap, roll_max_dry_spell))
      }
      else {
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > ", amount_rain, " & roll_max_dry_spell <= ", spell_max_dry_days, ") | is.na(x=", rain, ") | is.na(x=roll_sum_rain) | is.na(x=roll_max_dry_spell)"), sub_calculations=list(roll_sum_rain, roll_max_dry_spell))
      }
      
      # setting output options under this setting
      if ("doy" %in% output){
        start_of_rains_doy <- instatCalculations::instat_calculation$new(type="summary", function_exp=paste0("ifelse(test=is.na(x=dplyr::first(x=", rain, ")) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_max_dry_spell)), yes=NA, no=dplyr::first(x=", doy, ", default=NA))"), result_name="start_rain", save=2)
      }
      if ("date" %in% output){
        start_rain_date <- instatCalculations::instat_calculation$new(type="summary", function_exp=paste0("dplyr::if_else(condition=is.na(x=dplyr::first(x=", rain, ")) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_max_dry_spell)), true=as.Date(NA), false=dplyr::first(", date, ", default=NA))"), result_name="start_rain_date", save=2)
      }
      
    }
    if (dry_period){
      roll_sum_rain_dry_period <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("lead(x=RcppRoll::roll_suml(x=", rain, ", n=", period_max_dry_days, ", fill=NA))"), result_name="roll_sum_rain_dry_period", calculated_from=setNames(list(rain), data))
      n_dry_period <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("RcppRoll::roll_suml(x=roll_sum_rain_dry_period <= ", max_rain, ", n=", period_interval, " - ", period_max_dry_days, " + 1, fill=NA, na.rm=FALSE)"), result_name="n_dry_period", sub_calculations=list(roll_sum_rain_dry_period))
      # setting conditions filter under this setting
      if (total_rainfall && proportion){
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > wet_spell & n_dry_period == 0) | is.na(x=", rain, ") | is.na(x=roll_sum_rain) | is.na(x=n_dry_period)"), sub_calculations=list(roll_sum_rain, n_dry_period, total_rainfall_wet_spell))
      }
      else if (evaporation){
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > roll_sum_evap & n_dry_period == 0) | is.na(x=", rain, ") | is.na(x=roll_sum_rain) | is.na(x=n_dry_period)"), sub_calculations=list(roll_sum_rain, roll_sum_evap, n_dry_period))
      }
      else {
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > ", amount_rain, " & n_dry_period == 0) | is.na(x=", rain, ") | is.na(x=roll_sum_rain) | is.na(x=n_dry_period)"), sub_calculations=list(roll_sum_rain, n_dry_period))
      }
      
      # setting output options under this setting
      if ("doy" %in% output) {
        start_of_rains_doy <- instatCalculations::instat_calculation$new(type="summary", function_exp=paste0("ifelse(test=is.na(x=dplyr::first(x=", rain, ")) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=n_dry_period)), yes=NA, no=dplyr::first(x=", doy, ", default=NA))"), result_name="start_rain", save=2)
      }
      if ("date" %in% output){
        start_rain_date <- instatCalculations::instat_calculation$new(type="summary", function_exp=paste0("dplyr::if_else(condition=is.na(x=dplyr::first(x=", rain, ")) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=n_dry_period)), true=as.Date(NA), false=dplyr::first(", date, ", default=NA))"), result_name="start_rain_date", save=2)
      }        
    }
    
    # If all three of number_rain_days, dry_spells or dry_period is TRUE, set the appropriate conditions_filter function and the selected options for output
    if (number_rain_days && dry_spell && dry_period){
      roll_n_rain_days <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("RcppRoll::roll_sumr(x=rain_day, n=", rain_day_interval, ", fill=NA, na.rm=FALSE)"), result_name="roll_n_rain_days", sub_calculations=list(rain_day))
      dry_spell <- instatCalculations::instat_calculation$new(type="calculation", function_exp="instatClimatic::spells(x=rain_day == 0)", result_name="dry_spell", sub_calculations=list(rain_day))
      roll_max_dry_spell <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("dplyr::lead(x=RcppRoll::roll_maxl(n=", spell_interval, ", x=dry_spell, fill=NA))"), result_name="roll_max_dry_spell", sub_calculations=list(dry_spell))
      roll_sum_rain_dry_period <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("lead(x=RcppRoll::roll_suml(x=", rain, ", n=", period_max_dry_days, ", fill=NA))"), result_name="roll_sum_rain_dry_period", calculated_from=setNames(list(rain), data))
      n_dry_period <- instatCalculations::instat_calculation$new(type="calculation", function_exp=paste0("RcppRoll::roll_suml(x=roll_sum_rain_dry_period <= ", max_rain, ", n=", period_interval, " - ", period_max_dry_days, " + 1, fill=NA, na.rm=FALSE)"), result_name="n_dry_period", sub_calculations=list(roll_sum_rain_dry_period))
      # setting conditions filter under this setting
      if (total_rainfall && proportion) {
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > wet_spell & roll_n_rain_days >= ", min_rain_days, " & roll_max_dry_spell <= ", spell_max_dry_days, " & n_dry_period == 0) | is.na(x=", rain, ") | is.na(x=roll_sum_rain) | is.na(x=roll_n_rain_days) | is.na(x=roll_max_dry_spell) | is.na(x=n_dry_period)"), sub_calculations=list(roll_sum_rain, roll_n_rain_days, roll_max_dry_spell, n_dry_period, total_rainfall_wet_spell))
      }
      else if (evaporation){
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > roll_sum_evap & roll_n_rain_days >= ", min_rain_days, " & roll_max_dry_spell <= ", spell_max_dry_days, " & n_dry_period == 0) | is.na(x=", rain, ") | is.na(x=roll_sum_rain) | is.na(x=roll_n_rain_days) | is.na(x=roll_max_dry_spell) | is.na(x=n_dry_period)"), sub_calculations=list(roll_sum_rain, roll_sum_evap, roll_n_rain_days, roll_max_dry_spell, n_dry_period))
      }
      else {
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > ", amount_rain, " & roll_n_rain_days >= ", min_rain_days, " & roll_max_dry_spell <= ", spell_max_dry_days, " & n_dry_period == 0) | is.na(x=", rain, ") | is.na(x=roll_sum_rain) | is.na(x=roll_n_rain_days) | is.na(x=roll_max_dry_spell) | is.na(x=n_dry_period)"), sub_calculations=list(roll_sum_rain, roll_n_rain_days, roll_max_dry_spell, n_dry_period))
      }
      
      # setting output options under this setting
      if ("doy" %in% output){
        start_of_rains_doy <- instatCalculations::instat_calculation$new(type="summary", function_exp=paste0("ifelse(test=is.na(x=dplyr::first(x=", rain, ")) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_n_rain_days)) | is.na(x=dplyr::first(x=roll_max_dry_spell)) | is.na(x=dplyr::first(x=n_dry_period)), yes=NA, no=dplyr::first(x=", doy, ", default=NA))"), result_name="start_rain", save=2)
      }
      if ("date" %in% output){
        start_rain_date <- instatCalculations::instat_calculation$new(type="summary", function_exp=paste0("dplyr::if_else(condition=is.na(x=dplyr::first(x=", rain, ")) | is.na(x=dplyr::first(x=roll_sum_rain)) | is.na(x=dplyr::first(x=roll_n_rain_days)) | is.na(x=dplyr::first(x=roll_max_dry_spell)) | is.na(x=dplyr::first(x=n_dry_period)), true=as.Date(NA), false=dplyr::first(", date, ", default=NA))"), result_name="start_rain_date", save=2)
      }      
      
    }
    
    # If none of number_rain_days, dry_spells or dry_period is TRUE, set the appropriate conditions_filter function
    if (!number_rain_days && !dry_spell && !dry_period) {
      if (total_rainfall && proportion){
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > wet_spell) | is.na(x=", rain, ") | is.na(x=roll_sum_rain)"), sub_calculations=list(roll_sum_rain, total_rainfall_wet_spell))
      }
      else if (evaporation) {
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > roll_sum_evap) | is.na(x=", rain, ") | is.na(x=roll_sum_rain)"), sub_calculations=list(roll_sum_rain, roll_sum_evap))
      }
      else {
        conditions_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0("((", rain, " >= ", threshold, ") & roll_sum_rain > ", amount_rain, ") | is.na(x=", rain, ") | is.na(x=roll_sum_rain)"), sub_calculations=list(roll_sum_rain))
      }
    }
    
    grouping_by_year <- instatCalculations::instat_calculation$new(type="by", calculated_from=setNames(list(year), data))
    doy_filter <- instatCalculations::instat_calculation$new(type="filter", function_exp=paste0(doy, " >= 1 & ", doy, " <= 366"), calculated_from=databook::calc_from_convert(x=setNames(list(doy), data)))
    
    # If none of number_rain_days, dry_spells or dry_period is TRUE, set the selected options for output
    if (!number_rain_days && !dry_spell && !dry_period){
      if ("doy" %in% output){
        start_of_rains_doy <- instatCalculations::instat_calculation$new(type="summary", function_exp=paste0("ifelse(test=is.na(x=dplyr::first(x=", rain, ")) | is.na(x=dplyr::first(x=roll_sum_rain)), yes=NA, no=dplyr::first(x=", doy, ", default=NA))"), result_name="start_rain", save=2)
      }
      if ("data" %in% output){
        start_rain_date <- instatCalculations::instat_calculation$new(type="summary", function_exp=paste0("dplyr::if_else(condition=is.na(x=dplyr::first(x=", rain, ")) | is.na(x=dplyr::first(x=roll_sum_rain)), true=as.Date(NA), false=dplyr::first(", date, ", default=NA))"), result_name="start_rain_date", save=2)
      }
    }
    
    if ("status" %in% output){
      start_of_rains_status <- instatCalculations::instat_calculation$new(type="summary", function_exp=paste0("ifelse(n() > 0, ifelse(dplyr::first(is.na(roll_sum_rain)), NA, TRUE), FALSE)"), result_name="start_rain_status", save=2)
    }
    
    sub_cals <- list()
    if (exists("start_of_rains_doy")) sub_cals <- c(sub_cals, list(start_of_rains_doy))
    if (exists("start_rain_date")) sub_cals <- c(sub_cals, list(start_rain_date))
    if (exists("start_of_rains_status")) sub_cals <- c(sub_cals, list(start_of_rains_status))
    
    start_of_rains_combined <- instatCalculations::instat_calculation$new(type="combination", manipulations=list(conditions_filter, grouping_by_year, doy_filter), sub_calculation=sub_cals)
    data_book$run_instat_calculation(calc=start_of_rains_combined, display=FALSE, param_list=list(drop=FALSE))
    
    if ("status" %in% output){
      linked_data_name <- data_book$get_linked_to_data_name(data, link_cols=c(year))
      calculated_from_list <- c(setNames("start_rain_status", linked_data_name), setNames("start_rain", linked_data_name))
      start_rain_status2 <- instatCalculations::instat_calculation$new(type="calculation", function_exp="ifelse(!is.na(start_rain), TRUE, start_rain_status)", calculated_from=calculated_from_list, result_name="start_rain_status", save=2)
      start_rain_combined_status_2 <- instatCalculations::instat_calculation$new(type="combination", sub_calculations=list(start_rain_status2))
      data_book$run_instat_calculation(calc=start_rain_combined_status_2, display=FALSE, param_list=list(drop=drop))
    }
    
    data_book$convert_column_to_type(data_name=data, col_names=year, to_type=year_type)
    data_book$convert_linked_variable(from_data_frame=data, link_cols=c(year))
}