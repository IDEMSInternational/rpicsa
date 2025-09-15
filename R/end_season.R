#' End of the Season
#'
#' Calculate the last day (or date, or status) of the rainy season based on a simple bucket water‑balance model.
#'
#' @param data \code{character(1)} Name of the data frame in the \code{data_book} to work on.
#' @param date_time \code{character(1)} Name of the date or POSIXct column in \code{data}.
#' @param station \code{character(1)} Name of the station column, if data contain multiple stations. Defaults to \code{NULL} (no station grouping).
#' @param year \code{character(1)} Name of the year column. If \code{NULL}, it is auto‑created from \code{date_time} via \code{split_date()}.
#' @param rain \code{character(1)} Name of the rainfall column to use in the water balance.
#' @param doy \code{character(1)} Name of the day‑of‑year column. If \code{NULL}, it is auto‑created from \code{date_time}.
#' @param s_start_month \code{integer(1)} Month (1–12) to treat as the start of the water‑balance “year.” Defaults to \code{1} (January).
#' @param drop \code{logical(1)} If \code{TRUE} (default), drop years with only \code{NA} rainfall in the specified interval.
#' @param start_day \code{integer(1)} First day‑of‑year to include in the season calculation (1–365). Default \code{1}.
#' @param end_day \code{integer(1)} Last day‑of‑year to include (2–366). Default \code{366}.
#' @param output \code{character} Which outputs to produce. One or more of \code{"doy"}, \code{"date"}, \code{"status"}. Default all three.
#' @param capacity \code{numeric(1)} Soil water‐holding capacity (mm). Default \code{60}.
#' @param water_balance_max \code{numeric(1)} Threshold of water balance to end the season. Default \code{0.5}.
#' @param evaporation \code{character(1)} Whether to use a fixed evaporation \code{"value"} or a data‐column \code{"variable"}. Default \code{"value"}.
#' @param evaporation_value \code{numeric(1)} Daily evaporation (mm) if \code{evaporation = "value"}. Default \code{5}.
#' @param evaporation_variable \code{character(1)} Column name for daily evaporation if \code{evaporation = "variable"}.
#' @param reducing \code{logical(1)} If \code{TRUE}, apply a reducing‐bucket evaporation function. Default \code{FALSE}.
#' @param reducing_value \code{numeric(1)} Parameter controlling bucket reduction when \code{reducing = TRUE}. Default \code{0.5}.
#' @param data_book An existing \code{DataBook} object. If \code{NULL}, a new \code{DataBook} is created. Default \code{NULL}.
#'
#' @return Invisibly returns the used \code{data_book}; side effect is that the linked data frame  
#'   (e.g. \code{<data>_by_<station>_<year>}) is created with columns  
#'   \code{end_season}, \code{end_season_date}, and/or \code{end_season_status},  
#'   according to \code{output}.  
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
#' data_book$add_key("daily_data", "date", "key")
#' 
#'  # Run End-Rains:
#' end_season(data = "daily_data", date_time = "date", station = "station_name",
#'           year = "year", rain = "rain",
#'           start_day = 121, end_day = 300,
#'           output = "doy", data_book = data_book)
#'           
#'  # View output
#' daily_data_by_station_name_year <- data_book$get_data_frame("daily_data_by_station_name_year")
#' head(daily_data_by_station_name_year)
#'
#' @export
end_season <- function(data, date_time, station = NULL, year = NULL, rain = NULL,
                       doy = NULL,  s_start_month = 1, drop = TRUE,
                       start_day = 1, end_day = 366, output = c("doy", "date", "status"),
                       capacity = 60, water_balance_max = 0.5, evaporation = c("value", "variable"),
                       evaporation_value = 5, evaporation_variable = NULL, reducing = FALSE, reducing_value = 0.5,
                       data_book = NULL){
  
  if (is.null(data_book)) {
    data_book <- DataBook$new()
  }
  
  # Running checks
  checkmate::assert_string(data)
  checkmate::assert_string(rain)
  data_frame <- data_book$get_data_frame(data)
  assert_column_names(data_frame, rain)
  assert_column_names(data_frame, date_time)
  checkmate::assert(checkmate::check_date(data_frame[[date_time]], null.ok = TRUE), 
                    checkmate::check_posixct(data_frame[[date_time]],  null.ok = TRUE))
  checkmate::assert_string(station, null.ok = TRUE)
  checkmate::assert_string(year, null.ok = TRUE)
  checkmate::assert_string(doy, null.ok = TRUE)
  if (!is.null(station)) assert_column_names(data_frame, station)
  if (!is.null(year)) assert_column_names(data_frame, year)
  if (!is.null(doy)) assert_column_names(data_frame, doy)
  checkmate::assert_int(start_day, lower = 1, upper = 365)
  checkmate::assert_int(end_day, lower = 2, upper = 366)
  checkmate::assert_numeric(capacity, lower = 0)
  checkmate::assert_numeric(water_balance_max, lower = 0)
  checkmate::assert_numeric(evaporation_value, lower = 0)
  checkmate::assert_numeric(s_start_month, lower = 1, upper = 12, null.ok = TRUE)
  evaporation <- match.arg(evaporation)
  if (end_day <= start_day) stop("The `end_day` must be after the `start_day`")
  
  # calculate doy, year from date
  if (is.null(year)) {
    data_book$split_date(data_name = data, col_name = date_time, year_val = TRUE, s_start_month = s_start_month)
    year <- "year"
  }
  
  if (is.null(doy)){
    data_book$split_date(data_name = data, col_name = date_time, day_in_year_366 =TRUE, s_start_month = s_start_month)
    doy <- "doy"
  }
  
  # Setting up: Convert year to factor for `drop` to work successfully
  year_type <- data_book$get_column_data_types(data_name = data, columns = year)
  data_book$convert_column_to_type(data_name = data, col_names = year, to_type="factor")
  data_book$convert_linked_variable(from_data_frame = data, link_cols = c(year))
  
  # setting up for station too if needed
  if (!is.null(station)){
    data_book$convert_column_to_type(data_name = data, col_names = station, to_type="factor")
    data_book$convert_linked_variable(from_data_frame = data, link_cols = c(year, station))
  }
  
  # calculating water balance
  rain_min <- instatCalculations::instat_calculation$new(
    type="calculation", 
    function_exp = paste0("ifelse(test = is.na(x=", rain, "), yes = 0, no=", rain, ")"),
    result_name="rain_min",
    calculated_from = setNames(list(rain), data))
  rain_max <- instatCalculations::instat_calculation$new(
    type="calculation",
    function_exp = paste0("ifelse(test = is.na(x=", rain, "), yes = 100, no=", rain, ")"), # todo, should this yes = capacity?
    result_name="rain_max",
    calculated_from = setNames(list(rain), data))
  
  if (evaporation == "value"){
    if (!reducing){
      wb_min <- instatCalculations::instat_calculation$new(
        type="calculation",
        function_exp = paste0("purrr::accumulate(.f= ~ pmin(pmax(..1 + ..2, 0), ", capacity, "), .x = tail(x = rain_min - ", evaporation_value, ", n=-1), .init = 0)"),
        result_name="wb_min",
        sub_calculations = list(rain_min))
      wb_max <- instatCalculations::instat_calculation$new(
        type="calculation",
        function_exp = paste0("purrr::accumulate(.f= ~ pmin(pmax(..1 + ..2, 0), ", capacity, "), .x = tail(x = rain_max - ", evaporation_value, ", n=-1), .init = 0)"),
        result_name="wb_max",
        sub_calculations = list(rain_max))
    } else {
      wb_min <- instatCalculations::instat_calculation$new(
        type="calculation",
        function_exp = paste0("purrr::accumulate(.f= ~ pmin(pmax(..1 + ..2 - instatClimatic::WB_evaporation(..1, ", reducing_value, ", ", capacity, ", ", evaporation_value, ", ..2), 0), ", capacity, "), .x = tail(x = rain_min, n=-1), .init = 0)"),
        result_name="wb_min",
        sub_calculations = list(rain_min))
      wb_max <- instatCalculations::instat_calculation$new(
        type="calculation",
        function_exp = paste0("purrr::accumulate(.f= ~ pmin(pmax(..1 + ..2 - instatClimatic::WB_evaporation(..1, ", reducing_value, ", ", capacity, ", ", evaporation_value, ", ..2), 0), ", capacity, "), .x = tail(x = rain_max, n=-1), .init = 0)"),
        result_name="wb_max",
        sub_calculations = list(rain_max))
    }
  } else if (evaporation == "variable") {
    if (!reducing){
      wb_min <- instatCalculations::instat_calculation$new(
        type="calculation", 
        function_exp = paste0("purrr::accumulate(.x = tail(rain_min - as.numeric(", evaporation_variable, "), n=-1), .f= ~ pmin(pmax(..1 + ..2, 0), ", capacity, "), .init = 0)"), 
        result_name="wb_min", 
        sub_calculations = list(rain_min))
      wb_max <- instatCalculations::instat_calculation$new(
        type="calculation", 
        function_exp = paste0("purrr::accumulate(.f= ~ pmin(pmax(..1 + ..2, 0), ", capacity, "), .x = tail(x = rain_max - as.numeric(", evaporation_variable, "), n=-1), .init = 0)"), 
        result_name="wb_max", 
        sub_calculations = list(rain_max))
    } else {
      wb_min <- instatCalculations::instat_calculation$new(
        type="calculation", 
        function_exp = paste0("purrr::accumulate2(.x = tail(x = rain_min, n=-1), .y = tail(x=", evaporation_variable, ", n=-1), .f= ~ pmin(pmax(..1 + ..2 - instatClimatic::WB_evaporation(..1, ", reducing_value, ", ", capacity, ", ..3, ..2), 0), ", capacity, "), .init = 0)"), 
        result_name="wb_min", 
        sub_calculations = list(rain_min))
      wb_max <- instatCalculations::instat_calculation$new(
        type="calculation", 
        function_exp = paste0("purrr::accumulate2(.f= ~ pmin(pmax(..1 + ..2 - instatClimatic::WB_evaporation(..1, ", reducing_value, ", ", capacity, ", ..3, ..2), 0), ", capacity, "), .y = tail(x=", evaporation_variable, ", n=-1), .x = tail(x = rain_max, n=-1), .init = 0)"), 
        result_name="wb_max", 
        sub_calculations = list(rain_max))
    }
  }
  wb <- instatCalculations::instat_calculation$new(
    type="calculation",
    function_exp = paste0("ifelse(test=(wb_min != wb_max) | is.na(x=", rain, "), yes = NA, no = wb_min)"),
    result_name="wb",
    sub_calculations = list(wb_min, wb_max))
  conditions_check <- instatCalculations::instat_calculation$new(
    type="calculation",
    function_exp = paste0("ifelse((wb <=", water_balance_max, " | is.na(x=", rain, ")), 1, 0)"),
    result_name="conditions_check",
    sub_calculations = list(wb))
  conditions_filter <- instatCalculations::instat_calculation$new(
    type="filter",
    function_exp="conditions_check == 1",
    sub_calculations = list(conditions_check))
  
  # build the grouping vars
  group_vars <- c(station, year) %>% stats::na.omit()
  # turn into a named list: list("dodoma"="year", "dodoma"="station_name")
  calc_from <- setNames(
    as.list(group_vars), 
    rep(data, length(group_vars))
  )
  grouping_by_station_year <- instatCalculations::instat_calculation$new(
    type           = "by",
    calculated_from = calc_from
  )
  
  doy_filter <- instatCalculations::instat_calculation$new(
    type="filter",
    function_exp = paste0(doy, " >= ", start_day, "&", doy, " <= ", end_day),
    calculated_from = databook::calc_from_convert(x = setNames(list(doy), data)))
  
  if ("doy" %in% output){
    end_season <- instatCalculations::instat_calculation$new(
      type="summary",
      function_exp = paste0("ifelse(test = is.na(x = dplyr::first(x = wb)), yes = NA, no = dplyr::first(x=", doy, "))"),
      result_name="end_season",
      calculated_from = setNames(list(doy), data),
      save = 2)
  }
  
  if ("date" %in% output) {
    end_season_date <- instatCalculations::instat_calculation$new(
      type="summary", 
      function_exp = paste0("dplyr::if_else(condition = is.na(x = dplyr::first(x = wb)), true = as.Date(NA), false = dplyr::first(x=", date_time, "))"), 
      result_name="end_season_date", 
      save = 2)
  }
  
  first_sub_calcs <- list()
  if (exists("end_season")) first_sub_calcs <- c(first_sub_calcs, list(end_season))
  if (exists("end_season_date")) first_sub_calcs <- c(first_sub_calcs, list(end_season_date))
  
  end_of_season_combined <- instatCalculations::instat_calculation$new(
    type="combination",
    manipulations = list(conditions_filter, grouping_by_station_year, doy_filter),
    sub_calculations = first_sub_calcs)
  data_book$run_instat_calculation(display = FALSE, param_list = list(drop = drop), calc = end_of_season_combined)
  
  if ("status" %in% output){
    conditions_filter <- instatCalculations::instat_calculation$new(
      type="filter",
      function_exp="conditions_check == 1 | is.na(conditions_check) | conditions_check == 0",
      sub_calculations = list(conditions_check))
    end_season_status <- instatCalculations::instat_calculation$new(
      type="summary",
      function_exp="ifelse(dplyr::n() == 0, NA, ifelse(all(conditions_check == 0, na.rm = TRUE), FALSE, NA))",
      calculated_from = setNames(list(doy), data), result_name="end_season_status",
      sub_calculations = list(conditions_check), save = 2)
    end_of_season_combined_status <- instatCalculations::instat_calculation$new(
      type="combination",
      manipulations = list(conditions_filter, grouping_by_station_year, doy_filter),
      sub_calculations = list(end_season_status))
    data_book$run_instat_calculation(display = FALSE,
                                     param_list = list(drop = drop),
                                     calc = end_of_season_combined_status)
  }

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
  
  if ("status" %in% output){
    calculated_from_list <- c(setNames("end_season_status", linked_data_name), setNames("end_season", linked_data_name))
    end_season_status_2 <- instatCalculations::instat_calculation$new(
      type="calculation", 
      function_exp="ifelse(!is.na(end_season), TRUE, end_season_status)", 
      calculated_from = calculated_from_list, 
      result_name="end_season_status", 
      save = 2)
    end_season_combined_status_2 <- instatCalculations::instat_calculation$new(
      type="combination", 
      sub_calculations = list(end_season_status_2))
    data_book$run_instat_calculation(display = FALSE, param_list = list(drop = drop), calc = end_season_combined_status_2)
  }

  data_book$convert_column_to_type(data_name = data, col_names = year, to_type = year_type)
  data_book$convert_column_to_type(data_name = linked_data_name, col_names = year, to_type = year_type)
  
}