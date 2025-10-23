#' Start of the Rains
#'
#' Identify the first day, date, and/or status marking the start of the rainy season
#' according to a variety of definitions (single‐day threshold, total rainfall over
#' days, proportion, spell length, etc.).
#'
#' @param data \code{character(1)} Name of the data frame in the \code{data_book}.
#' @param date_time \code{character(1)} Name of the date (Date or POSIXct) column in \code{data}.
#' @param station \code{character(1)} Name of the station column, if multiple stations are present. Default \code{NULL}.
#' @param year \code{character(1)} Name of the year column. If \code{NULL}, it will be created from \code{date_time}.
#' @param rain \code{character(1)} Name of the rainfall column (mm) to evaluate.
#' @param threshold \code{numeric(1)} Minimum rainfall on a single day to count as “rain.” Default \code{0.85} mm.
#' @param doy \code{character(1)} Name of the day‐of‐year column. If \code{NULL}, it will be created from \code{date_time}.
#' @param start_day \code{integer(1)} Earliest day of year (1–366) to consider. Default \code{1}.
#' @param end_day \code{integer(1)} Latest day of year (1–366) to consider. Default \code{366}.
#' @param s_start_month \code{integer(1)} Month (1–12) to treat as the start of the “year” when creating \code{year} or \code{doy}. Default \code{NULL} (assumes January).
#' @param drop \code{logical(1)} If \code{TRUE}, drop years with no valid start‐rain calculation. Default \code{TRUE}.
#' @param output \code{character} Which outputs to produce: one or more of \code{"doy"}, \code{"date"}, \code{"status"}. Default all three.
#' @param total_rainfall_over_days \code{integer(1)} Number of days over which to total rainfall for the “total rainfall” definition. Default \code{1}.
#' @param total_rainfall_comparison \code{character} Method for multi‐day definition: \code{"amount"}, \code{"proportion"}, or \code{"evaporation"}. Default \code{"amount"}.
#' @param amount_rain \code{numeric(1)} If \code{total_rainfall_comparison="amount"}, the total rainfall (mm) threshold. Default \code{20}.
#' @param prob_rain_day \code{numeric(1)} If \code{total_rainfall_comparison="proportion"}, the quantile (0–1) of the rolling total to use. Default \code{0.8}.
#' @param evaporation_variable \code{character(1)} Name of evaporation (mm) column, if using \code{"evaporation"}. Default \code{NULL}.
#' @param fraction \code{numeric(1)} Multiplier (0–1) applied to evaporation values when \code{total_rainfall_comparison="evaporation"}. Default \code{0.5}.
#' @param number_rain_days \code{logical(1)} If \code{TRUE}, define start by minimum rainy days in an interval. Default \code{FALSE}.
#' @param min_rain_days \code{integer(1)} Minimum count of rainy days if \code{number_rain_days=TRUE}. Default \code{1}.
#' @param rain_day_interval \code{integer(1)} Interval length (days) for \code{number_rain_days}. Default \code{2}.
#' @param dry_spell \code{logical(1)} If \code{TRUE}, define start by a maximum dry‐spell length. Default \code{FALSE}.
#' @param spell_interval \code{integer(1)} Interval length (days) for the dry‐spell test. Default \code{21}.
#' @param spell_max_dry_days \code{integer(1)} Maximum consecutive dry days if \code{dry_spell=TRUE}. Default \code{9}.
#' @param dry_period \code{logical(1)} If \code{TRUE}, define start by combining max rainfall and max dry days in a period. Default \code{FALSE}.
#' @param period_interval \code{integer(1)} Interval length (days) for \code{dry_period}. Default \code{45}.
#' @param max_rain \code{numeric(1)} Maximum total rainfall (mm) allowed in \code{period_interval} if \code{dry_period=TRUE}. Default \code{40}.
#' @param period_max_dry_days \code{integer(1)} Maximum dry days if \code{dry_period=TRUE}. Default \code{30}.
#' @param data_book A \code{DataBook} object to use. If \code{NULL}, a new one is created. Default \code{NULL}.
#'
#' @return Invisibly returns the used \code{data_book}.  The side effect is that a new
#'   linked data frame (e.g.\ \code{<data>_by_<station>_<year>}) is created with columns
#'   \code{start_rain}, \code{start_rain_date}, and/or \code{start_rain_status}.
#'
#' @export
#' 
#' @examples
#' library(databook)
#' data_book <- DataBook$new()
#' daily_data <- rpicsa::daily_niger %>%
#'   dplyr::filter(year <= 1950) %>%
#'   dplyr::filter(year > 1945) %>%
#'   dplyr::mutate(year = as.numeric(year)) %>%
#'   dplyr::filter(station_name == "Agades")
#' data_book$import_data(list(daily_data = daily_data))
#' 
#' daily_data <- data_book$get_data_frame("daily_data")
#' 
#' suppressWarnings(start_rains(data = "daily_data",
#'                              date_time = "date",
#'                              station = "station_name",
#'                              year = "year",
#'                              rain = "rain",
#'                              start_day = 121,
#'                              end_day = 300,
#'                              output = c("doy", "date", "status"),
#'                              data_book = data_book))
#' daily_data_by_station_name_year <- data_book$get_data_frame("daily_data_by_station_name_year")

start_rains <- function(data, date_time, station = NULL, year = NULL, rain, threshold = 0.85,
                        doy = NULL, start_day = 1, end_day = 366, s_start_month = 1,
                        drop = TRUE,
                        output = c("doy", "date", "status"),
                        total_rainfall_over_days = 1,
                        total_rainfall_comparison = c("amount", "proportion", "evaporation"),
                        amount_rain = 20, prob_rain_day = 0.8,
                        evaporation_variable = NULL, fraction = 0.5, 
                        number_rain_days = FALSE, min_rain_days = 1, rain_day_interval = 2,
                        dry_spell = FALSE, spell_interval = 21, spell_max_dry_days = 9,
                        dry_period = FALSE, period_interval = 45, max_rain = 40, period_max_dry_days = 30,
                        data_book = data_book) {
  
  total_rainfall_comparison <- match.arg(total_rainfall_comparison)
  
  # creating the a new databook object if it doesn't exist
  if (is.null(data_book)){
    data_book = DataBook$new()
  }
  
  # Running checks
  checkmate::assert_string(data)
  checkmate::assert_string(date_time)
  checkmate::assert_string(rain)
  checkmate::assert_string(station, null.ok = TRUE)
  checkmate::assert_string(year, null.ok = TRUE)
  checkmate::assert_string(doy, null.ok = TRUE)
  data_frame <- data_book$get_data_frame(data)
  assert_column_names(data_frame, rain)
  assert_column_names(data_frame, date_time)
  checkmate::assert(checkmate::check_date(data_frame[[date_time]], null.ok = TRUE), 
                    checkmate::check_posixct(data_frame[[date_time]],  null.ok = TRUE))
  if (!is.null(station)) assert_column_names(data_frame, station)
  if (!is.null(year)) assert_column_names(data_frame, year)
  if (!is.null(doy)) assert_column_names(data_frame, doy)
  
  checkmate::assert_numeric(threshold, lower = 0)
  checkmate::assert_int(start_day, lower = 1, upper = 365)
  checkmate::assert_int(end_day, lower = 2, upper = 366)
  checkmate::assert_numeric(s_start_month, lower = 1, upper = 12)
  checkmate::assert_logical(drop)
  checkmate::assert_character(output)
  checkmate::assert_int(total_rainfall_over_days, lower = 1)
  checkmate::assert_string(total_rainfall_comparison)
  
  if (total_rainfall_comparison == "amount"){
    checkmate::assert_int(amount_rain, lower = 0)
  }
  if (total_rainfall_comparison == "proportion"){
    checkmate::assert_number(prob_rain_day, lower = 0, upper = 1)
  }
  if (total_rainfall_comparison == "evaporation"){
    checkmate::assert_number(fraction, lower = 0, upper = 1)
    checkmate::assert_string(evaporation_variable)
    assert_column_names(data_frame, evaporation_variable)
  }
  
  # Checks for the three other options
  checkmate::assert_logical(number_rain_days)
  checkmate::assert_logical(dry_spell)
  checkmate::assert_logical(dry_period)
  
  if (number_rain_days){
    checkmate::assert_int(min_rain_days, lower = 0)
    checkmate::assert_int(rain_day_interval, lower = 1)
    if (rain_day_interval < min_rain_days)
      stop("Value given in `rain_day_interval` must be equal to or greater than the value given in `min_rain_days`")
  }
  if (dry_spell){
    checkmate::assert_int(spell_interval, lower = 1)
    checkmate::assert_int(spell_max_dry_days, lower = 0)
    if (spell_interval < spell_max_dry_days)
      stop("Value given in `spell_interval` must be equal to or greater than the value given in `spell_max_dry_days`")
  }
  if (dry_period){
    checkmate::assert_int(period_interval, lower = 1)
    checkmate::assert_int(max_rain, lower = 0)
    checkmate::assert_int(period_max_dry_days, lower = 0)
    if (period_interval < period_max_dry_days)
      stop("Value given in `period_interval` must be equal to or greater than the value given in `period_max_dry_days`")
  }

  # calculate doy, year from date
  if(is.null(year)){#if(!year %in% names(data)) { # do instead of is.null because of epicsawrap. we always read in "year" whether it exists or not.
    data_book$split_date(data_name = data,
                         col_name = date_time,
                         year_val = TRUE,
                         s_start_month = s_start_month)
    year <- "year"
  }
  if(is.null(doy)){ 
    data_book$split_date(data_name = data,
                         col_name = date_time,
                         day_in_year_366 =TRUE,
                         s_start_month = s_start_month)
    doy <- "doy"
  }
  
  # Setting up the codes: transferring to factor for .drop purposes.
  year_type <- data_book$get_column_data_types(data_name=data, columns=year)
  data_book$convert_column_to_type(data_name=data, col_names=year, to_type="factor")
  data_book$convert_linked_variable(from_data_frame=data, link_cols=c(year))
  
  if (!is.null(station)) {
    station_type <- data_book$get_column_data_types(data_name=data, columns=station)
    data_book$convert_column_to_type(data_name = data, col_names = station, to_type="factor")
    data_book$convert_linked_variable(from_data_frame=data, link_cols=c(year, station))
    grouping_by_station <- instatCalculations::instat_calculation$new(
      type="by", 
      calculated_from=setNames(list(station), data))
  }
  
  ### Set up the Conditions and Calculations ----------------------------------------------------------
  
  # need rain >= threshold for it to be a rainy day (can't have SoR on a day without rains)
  rain_day <- instatCalculations::instat_calculation$new(
    type="calculation", 
    function_exp=paste0(rain, " >= ", threshold), 
    result_name="rain_day", 
    calculated_from=setNames(list(rain), data))
  
  ##### If evaporation, then we have additional argument 
  if (total_rainfall_comparison == "evaporation") {
    fraction_evap <- instatCalculations::instat_calculation$new(
      type="calculation", 
      function_exp=paste0(evaporation_variable, " * ", fraction), 
      result_name="fraction_evap", 
      calculated_from=setNames(list(evaporation_variable), data))
    roll_sum_evap <- instatCalculations::instat_calculation$new(
      type="calculation", 
      function_exp=paste0("RcppRoll::roll_sumr(x=fraction_evap, n=", total_rainfall_over_days, ", fill=NA, na.rm=FALSE)"), 
      result_name="roll_sum_evap", 
      sub_calculations=list(fraction_evap))
  }
  
  # TODO: better way to do this without repeating code. 
  if (!is.null(station)){
    roll_sum_rain <- instatCalculations::instat_calculation$new(
      type="calculation", 
      function_exp=paste0("RcppRoll::roll_sumr(x=", rain, ", n=", total_rainfall_over_days, ", fill=NA, na.rm=FALSE)"), 
      result_name="roll_sum_rain", 
      calculated_from=setNames(list(rain), data), 
      manipulations=list(grouping_by_station))
  } else {
    roll_sum_rain <- instatCalculations::instat_calculation$new(
      type="calculation", 
      function_exp=paste0("RcppRoll::roll_sumr(x=", rain, ", n=", total_rainfall_over_days, ", fill=NA, na.rm=FALSE)"), 
      result_name="roll_sum_rain", 
      calculated_from=setNames(list(rain), data))
  }
  
  # Then if proportion is used:
  if (total_rainfall_comparison == "proportion"){
    total_rainfall_wet_spell <- instatCalculations::instat_calculation$new(
      type="calculation", 
      function_exp=paste0("quantile(x=roll_sum_rain, probs=", prob_rain_day, ", na.rm=TRUE)"), 
      result_name="wet_spell", 
      sub_calculations=list(roll_sum_rain))
  }
  
  # Setting up the Conditions Filter String: 
  conditions_filter_and_part <- paste0("(", rain, " >= ", threshold, ")")
  conditions_filter_or_part <- paste0("is.na(x = ", rain, ") | is.na(x=roll_sum_rain)")
  
  # TODO
  conditions_sub_calcs <- list()
  conditions_sub_calcs <- c(conditions_sub_calcs, list(roll_sum_rain))
  
  if (total_rainfall_comparison == "amount"){
    conditions_filter_and_part <- paste0(conditions_filter_and_part, " & roll_sum_rain > ", amount_rain)
  } else if (total_rainfall_comparison == "proportion"){
    conditions_filter_and_part <- paste0(conditions_filter_and_part, " & roll_sum_rain > wet_spell")
    conditions_sub_calcs <- c(conditions_sub_calcs, list(total_rainfall_wet_spell))
  } else {
    conditions_filter_and_part <- paste0(conditions_filter_and_part, " & roll_sum_rain > roll_sum_evap")
    conditions_sub_calcs <- c(conditions_sub_calcs, list(roll_sum_evap))
  }
  
  # for the summary calculation: "test = ..." part of the ifelse statement
  output_doy_test_exp <- paste0("is.na(x=dplyr::first(x = ", rain, ")) | is.na(x=dplyr::first(x = roll_sum_rain))")
  
  ### OPTION 1: For Number of Rainy Days - we have rain_day_interval and roll_n_rain_days
  if (number_rain_days){
    roll_n_rain_days <- instatCalculations::instat_calculation$new(
      type="calculation", 
      function_exp=paste0("RcppRoll::roll_sumr(x=rain_day, n=", rain_day_interval, ", fill=NA, na.rm=FALSE)"), 
      result_name="roll_n_rain_days", 
      sub_calculations=list(rain_day))
    
    # For the conditions filter
    conditions_filter_and_part <- paste0(conditions_filter_and_part, " & roll_n_rain_days >= ", min_rain_days)
    conditions_filter_or_part <- paste0(conditions_filter_or_part, " | is.na(x = roll_n_rain_days)")
    conditions_sub_calcs <- c(conditions_sub_calcs, list(roll_n_rain_days))
  
    # For the summary calculation
    output_doy_test_exp <- paste0(output_doy_test_exp, "| is.na(x=dplyr::first(x=roll_n_rain_days))")
  }
  
  ### OPTION 2: For Dry Spell - 
  if (dry_spell){
    dry_spell <- instatCalculations::instat_calculation$new(
      type="calculation", 
      function_exp="instatClimatic::spells(x=rain_day == 0)", 
      result_name="dry_spell", 
      sub_calculations=list(rain_day))
    roll_max_dry_spell <- instatCalculations::instat_calculation$new(
      type="calculation", 
      function_exp=paste0("dplyr::lead(x=RcppRoll::roll_maxl(n = ", spell_interval, ", x = dry_spell, fill = NA))"), 
      result_name="roll_max_dry_spell", 
      sub_calculations=list(dry_spell))
    
    # For the conditions filter
    conditions_filter_and_part <- paste0(conditions_filter_and_part, " & roll_max_dry_spell <= ", spell_max_dry_days)
    conditions_filter_or_part <- paste0(conditions_filter_or_part, " | is.na(x = roll_max_dry_spell)")
    conditions_sub_calcs <- c(conditions_sub_calcs, list(roll_max_dry_spell))

    # For the summary calculation
    output_doy_test_exp <- paste0(output_doy_test_exp, "| is.na(x=dplyr::first(x=roll_max_dry_spell))")
  }
  
  ### OPTION 3: For Dry Period -
  if (dry_period){
    roll_sum_rain_dry_period <- instatCalculations::instat_calculation$new(
      type="calculation", 
      function_exp=paste0("lead(x=RcppRoll::roll_suml(x = ", rain, ", n = ", period_max_dry_days, ", fill = NA))"), 
      result_name="roll_sum_rain_dry_period", 
      calculated_from=setNames(list(rain), data))
    n_dry_period <- instatCalculations::instat_calculation$new(
      type="calculation", 
      function_exp=paste0("RcppRoll::roll_suml(x = roll_sum_rain_dry_period <= ", max_rain, ", n=", period_interval, " - ", period_max_dry_days, " + 1, fill = NA, na.rm = FALSE)"), 
      result_name="n_dry_period", 
      sub_calculations=list(roll_sum_rain_dry_period))
    
    # For the conditions filter
    conditions_filter_and_part <- paste0(conditions_filter_and_part, " & n_dry_period == 0")
    conditions_filter_or_part <- paste0(conditions_filter_or_part, " | is.na(x = n_dry_period)")
    conditions_sub_calcs <- c(conditions_sub_calcs, list(n_dry_period))
    
    # For the summary calculation
    output_doy_test_exp <- paste0(output_doy_test_exp, "| is.na(x=dplyr::first(x=n_dry_period))")
  }
  
  # For the conditions filter (function_exp)
  
  ### Set up the Filters ----------------------------------------------------------
  conditions_filter_function_exp <- paste0("(", conditions_filter_and_part, ") | ", conditions_filter_or_part)
  
  conditions_filter <- instatCalculations::instat_calculation$new(
    type = "filter",
    function_exp = conditions_filter_function_exp,
    sub_calculations = conditions_sub_calcs)
  
  grouping_by_year <- instatCalculations::instat_calculation$new(
    type="by", 
    calculated_from=setNames(list(year), data))
  doy_filter <- instatCalculations::instat_calculation$new(
    type="filter", 
    function_exp=paste0(doy, " >= ", start_day, " & ", doy, " <= ", end_day), 
    calculated_from=databook::calc_from_convert(x=setNames(list(doy), data)))
  
  # The summary calculation --------------------------------------------------------
  # For the summary calculation
  if ("doy" %in% output){
    output_doy_function_exp <- paste0("ifelse(test = (", output_doy_test_exp, "), yes=NA, no=dplyr::first(x = ", doy, ", default=NA))")
    start_of_rains_doy <- instatCalculations::instat_calculation$new(
      type = "summary", 
      function_exp = output_doy_function_exp, 
      result_name = "start_rain", 
      save = 2)
  }
  if ("date" %in% output){
    output_date_function_exp <- paste0("dplyr::if_else(condition = (", output_doy_test_exp, "), true = as.Date(NA), false = dplyr::first(", date_time, ", default=NA))")
    start_rain_date <- instatCalculations::instat_calculation$new(
      type = "summary", 
      function_exp = output_date_function_exp, 
      result_name = "start_rain_date", 
      save = 2)
  }
  if ("status" %in% output){
    start_of_rains_status <- instatCalculations::instat_calculation$new(
      type="summary", 
      function_exp=paste0("ifelse(dplyr::n() > 0, ifelse(dplyr::first(is.na(roll_sum_rain)), NA, TRUE), FALSE)"), 
      result_name="start_rain_status", 
      save=2)
  }
  
  sub_calcs <- list()
  if (exists("start_of_rains_doy")) sub_calcs <- c(sub_calcs, list(start_of_rains_doy))
  if (exists("start_rain_date")) sub_calcs <- c(sub_calcs, list(start_rain_date))
  if (exists("start_of_rains_status")) sub_calcs <- c(sub_calcs, list(start_of_rains_status))
  
  # Run calculation ----------------------------------------------------------------
  start_of_rains_combined <- instatCalculations::instat_calculation$new(
    type="combination", 
    manipulations=list(conditions_filter, grouping_by_year, doy_filter), 
    sub_calculation=sub_calcs)
  data_book$run_instat_calculation(
    calc=start_of_rains_combined, 
    display=FALSE, 
    param_list=list(drop=drop))
  
  # pick the right link columns
  link_cols <- if (is.null(station)) {
    year
  } else {
    c(station, year)
  }
  
  linked_data_name <- data_book$get_linked_to_data_name(
    data, link_cols
  )
  
  ### Additions to status: ---------------------------------------------------------
  if ("status" %in% output){
    calculated_from_list <- c(setNames("start_rain_status", linked_data_name), setNames("start_rain", linked_data_name))
    start_rain_status2 <- instatCalculations::instat_calculation$new(
      type="calculation", 
      function_exp="ifelse(!is.na(start_rain), TRUE, start_rain_status)", 
      calculated_from=calculated_from_list, 
      result_name="start_rain_status", 
      save=2)
    start_rain_combined_status_2 <- instatCalculations::instat_calculation$new(
      type="combination", 
      sub_calculations=list(start_rain_status2))
    data_book$run_instat_calculation(
      calc=start_rain_combined_status_2, 
      display=FALSE, 
      param_list=list(drop=drop))
  }
  
  data_book$convert_column_to_type(data_name = data, col_names = year, to_type = year_type)
  data_book$convert_column_to_type(data_name = linked_data_name, col_names = year, to_type = year_type)
  
  if (!is.null(station)){
    data_book$remove_unused_station_year_combinations(data_name=data, year=year, station=station)
    data_book$convert_linked_variable(from_data_frame=data, link_cols=link_cols) 
  } 
}
