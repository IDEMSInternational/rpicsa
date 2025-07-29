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
#' @param interval_length \code{numerical(1)} Number of days for the minimum rainfall to fall in.
#' @param min_rainfall \code{numerical(1)} Minimum amount of rainfall to occur on the set of days defined in `interval_length`.
#' @param data_book The data book object where the data object is stored, default `NULL`.
#'
#' @return A data.frame with the day of year and/or date for the end of the rains for each year (and station).
#' @export
#'
#' @examples
#' 
#' # Call in databook package and set up the environment
#' library(databook)
#' data_book <- DataBook$new()
#' 
#' # Filter our data so we are only considering a small portion of it
#' daily_data <- rpicsa::daily_niger %>%
#'   dplyr::filter(year <= 1950) %>%
#'   dplyr::filter(year > 1945) %>%
#'   dplyr::mutate(year = as.numeric(year)) %>%
#'   dplyr::filter(station_name == "Agades")
#' data_book$import_data(list(daily_data = daily_data))
#' 
#'  # Run End-Rains:
#' end_rains(data = "daily_data", date_time = "date", station = "station_name",
#'           year = "year", rain = "rain",
#'           start_day = 121, end_day = 300,
#'           output = "doy", data_book = data_book)
#'           
#'  # View output
#' daily_data_by_station_name_year <- data_book$get_data_frame("daily_data_by_station_name_year")
#' head(daily_data_by_station_name_year)
end_rains <- function(data, date_time, station = NULL, year = NULL, rain = NULL,
                      doy = NULL,  s_start_doy = NULL, drop = TRUE,
                      start_day = 1, end_day = 366, output = c("doy", "date", "status"),
                      interval_length = 1, min_rainfall = 10, data_book = NULL) {
  
  if (is.null(data_book)) {
    data_book <- DataBook$new()
  }
  # 1. Checks
  checkmate::assert_character(data)
  data_frame <- data_book$get_data_frame(data)
  checkmate::assert_character(rain)
  assert_column_names(data_frame, rain)
  checkmate::assert(checkmate::check_date(data_frame[[date_time]], null.ok = TRUE), 
                    checkmate::check_posixct(data_frame[[date_time]],  null.ok = TRUE))
  checkmate::assert_string(station, null.ok = TRUE)
  checkmate::assert_string(year, null.ok = TRUE)
  checkmate::assert_string(doy, null.ok = TRUE)
  checkmate::assert_numeric(s_start_doy, lower = 1, upper = 366, null.ok = TRUE)
  # if (!is.null(station)) assert_column_names(data_frame, station)
  # if (!is.null(date_time)) assert_column_names(data_frame, date_time)
  # if (!is.null(year)) assert_column_names(data_frame, year)
  # if (!is.null(doy)) assert_column_names(data_frame, doy)
  checkmate::assert_int(start_day, lower = 1, upper = 365)
  checkmate::assert_int(end_day, lower = 2, upper = 366)
  checkmate::assert_int(interval_length, lower = 1)
  checkmate::assert_int(min_rainfall, lower = 0)

  # 3. Add in R code to create DOY and Year if they are NULL (like in summary_temp)
  if (is.null(year)) {
    data_book$split_date(data_name=data, col_name=date_time, year_val=TRUE, s_start_month=1)
    year <- "year"
  }
  
  if (is.null(doy)){
    data_book$split_date(data_name = data, col_name=date_time, day_in_year_366 =TRUE, s_start_month=1)
    doy <- "doy"
  }
  
  # 4. We use the Calculation System
  # to avoid dropping levels, set as factor
  year_type <- data_book$get_column_data_types(data_name=data, columns=year)
  data_book$convert_column_to_type(data_name=data, col_names=year, to_type="factor")
  data_book$convert_linked_variable(from_data_frame=data, link_cols=c(year))
  
  # run end of rains r code
  roll_sum_rain <- instatCalculations::instat_calculation$new(
    type="calculation",
    function_exp=paste0("RcppRoll::roll_sumr(x=rain, n = ", interval_length, ", fill=NA, na.rm=FALSE)"),
    result_name="roll_sum_rain",
    calculated_from=setNames(list(rain), data))
  conditions_filter <- instatCalculations::instat_calculation$new(
    type="filter",
    function_exp=paste0("(roll_sum_rain > ", min_rainfall, ") | is.na(x=roll_sum_rain)"),
    sub_calculations=list(roll_sum_rain))
  
  # build the grouping vars
  group_vars <- c(station, year) %>% stats::na.omit()
  # turn into a named list: list("dodoma"="year", "dodoma"="station_name")
  calc_from <- setNames(
    as.list(group_vars), 
    rep(data, length(group_vars))
  )
  grouping_by_station_year <- instatCalculations::instat_calculation$new(
    type            = "by",
    calculated_from = calc_from
  )
  
  doy_filter <- instatCalculations::instat_calculation$new(
    type="filter",
    function_exp=paste0("doy >=", start_day, " & doy <= ", end_day),
    calculated_from=databook::calc_from_convert(x=setNames(list(doy), data)))
  
  if ("doy" %in% output){
    end_rains <- instatCalculations::instat_calculation$new(
      type="summary",
      function_exp=paste0("ifelse(test=is.na(x=dplyr::last(x=roll_sum_rain)), yes=NA, no=dplyr::last(x=", doy, "))"),
      result_name="end_rains",
      calculated_from=setNames(list(doy), data),
      save=2)
  }
  if ("date" %in% output){
    end_rains_date <- instatCalculations::instat_calculation$new(
      type="summary",
      function_exp=paste0("dplyr::if_else(condition=is.na(x=dplyr::last(x=roll_sum_rain)), true=as.Date(NA), false=dplyr::last(x=", date_time, "))"),
      result_name="end_rains_date",
      calculated_from=setNames(list(date_time), data),
      save=2)
  } 
  if ("status" %in% output){
    end_rains_status <- instatCalculations::instat_calculation$new(
      type="summary",
      function_exp="ifelse(dplyr::n() > 0, yes=ifelse(is.na(x=dplyr::last(x=roll_sum_rain)), yes=NA, no=TRUE), no=FALSE)",
      result_name="end_rains_status",
      save=2)
  } 
  
  # Now collect only the ones you created into a list:
  sub_calcs <- list()
  if (exists("end_rains"))         sub_calcs <- c(sub_calcs, list(end_rains))
  if (exists("end_rains_date"))    sub_calcs <- c(sub_calcs, list(end_rains_date))
  if (exists("end_rains_status"))  sub_calcs <- c(sub_calcs, list(end_rains_status))
  
  # In Combined, we run all that were checked out of end_rains, end_rains_date, end_rains_status and put them in sub_calculations
  end_of_rains_combined <- instatCalculations::instat_calculation$new(type="combination",
                                                                      manipulations=list(conditions_filter, grouping_by_station_year, doy_filter),
                                                                      sub_calculations = sub_calcs)
  
  data_book$run_instat_calculation(display=FALSE,
                                   param_list=list(drop = drop),
                                   calc=end_of_rains_combined)
  
  # pick the right link columns
  link_cols <- if (is.null(station)) {
    year
  } else {
    c(station, year)
  }
  
  # this will now return a single name
  linked_data_name <- data_book$get_linked_to_data_name(
    data, link_cols
  )
  
  data_book$convert_column_to_type(data_name=data, col_names=year, to_type=year_type)
  
  # now safely convert that one df
  data_book$convert_column_to_type(
    data_name = linked_data_name,
    col_names = year,
    to_type   = year_type
  )
  if (!is.null(station)) data_book$remove_unused_station_year_combinations(data_name=data, year=year, station=station)
}
