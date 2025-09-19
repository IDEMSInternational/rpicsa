#' Get Spells Data
#' 
#' This function calculates rainfall or climatic spells
#' 
#' @param data A data frame containing the data to be analysed.
#' @param summary_data \code{character(1)} Name of the summary data frame in \code{data_book}
#'   that contains the season boundary columns (e.g. \code{start_rain}, \code{end_rains}).
#' @param date_time \code{character(1)} Column name (in \code{data}) of the date variable.
#' @param year \code{character(1)} Column name (in \code{data}) of the year variable.
#'   If \code{NULL}, it is created from \code{date_time} using \code{lubridate::year()} with
#'   \code{s_start_month} applied.
#' @param element The name of the column in 'data' for which extremes are to be found.
#' @param station The name of the `station` column in 'data'. Default `NULL`.
#' @param s_start_month \code{integer(1)} Month (1–12) to treat as the start of the “statistical year”
#'   when deriving \code{year} or \code{doy} from \code{date_time}. Default \code{1} (January).
#' @param doy \code{character(1)} The name of the day of year column in \code{data} to apply the function to. If \code{NULL} it will be created using the \code{date_time} variable.
#' @param start_date \code{character(1)} Column name in \code{summary_data} giving the start of rains.
#' @param end_date \code{character(1)} Column name in \code{summary_data} giving the end of season.
#' @param day_from A numeric value for the start day in the year (numeric, 1–366) to use if `start_date` is `NULL`. 
#' @param day_to A numeric value for the end day in the year (numeric, 1–366) to use if `end_date` is `NULL`. 
#' @param direction A character string specifying the direction for the operation. It can be either `"greater"`, `"less"`, `"between"`, or `"outer"`.
#' @param value A numeric value specifying the threshold value (e.g., 50 mm for rainfall). This is then the upper bound value if `direction == "between"` or `direction == "outer"`.
#' @param lb_value A numeric value for the lower bound if `direction == "between"` or `direction == "outer"`.
#' @param data_book The \code{DataBook} (R6) object holding \code{data} and \code{summary_data}.
#' 
#' @export
#' @return The calculated spells
#' 


get_spells_data <- function(data, element, summary_data = NULL, date_time = NULL, year = NULL, station = NULL, 
                            s_start_month = 1,doy = NULL, start_date = NULL, end_date = NULL, day_from = 1, 
                            day_to = 366, direction = c("greater", "less", "between", "outer"), value = 0.85, 
                            lb_value = 0, data_book = data_book){

    if (is.null(data_book)){
        data_book <- DataBook$new()
    }
  
    direction <- match.arg(direction)
    
    # Running checks for data
    checkmate::assert_string(data)
    checkmate::assert_string(element)
    checkmate::assert_string(date_time, null.ok=TRUE)
    checkmate::assert_string(year, null.ok = TRUE)
    checkmate::assert_string(station, null.ok = TRUE)
    checkmate::assert_string(doy, null.ok = TRUE)
    checkmate::assert_string(start_date, null.ok = TRUE)
    checkmate::assert_string(end_date, null.ok = TRUE)
    data_frame <- data_book$get_data_frame(data)
    assert_column_names(data_frame, element)
    checkmate::assert(checkmate::check_date(data_frame[[date_time]]), 
                      checkmate::check_posixct(data_frame[[date_time]]))
    if (!is.null(date_time)) assert_column_names(data_frame, date_time)
    if (!is.null(year)) assert_column_names(data_frame, year)
    if (!is.null(station)) assert_column_names(data_frame, station)
    if (!is.null(doy)) assert_column_names(data_frame, doy)
    
    checkmate::assert_int(s_start_month, lower = 1, upper = 12)
    checkmate::assert_int(day_from)
    checkmate::assert_int(day_to, lower = 1, upper = 12)
    checkmate::assert_numeric(value, lower = 0)
    checkmate::assert_numeric(lb_value, lower = 0)
    checkmate::assert_string(direction)
    
    if (is.null(year)) {
        data_book$split_date(data_name = data, col_name = date_time, year_val = TRUE, s_start_month = s_start_month)
        year <- "year"
    }
    if (is.null(doy)){
        data_book$split_date(data_name = data, col_name=date_time, day_in_year_366 =TRUE, s_start_month = s_start_month)
        doy <- "doy"
    }
    
    fn_exps <- dplyr::case_match(
        direction,
        "greater" ~ paste0("(", element, " >= ", value, ")"),
        "less"    ~ paste0("(", element, " <= ", value, ")"),
        "between" ~ paste0("(", element, " >= ", lb_value, ") & (", element, " <= ", value, ")"),
        "outer"   ~ paste0("(", element, " <= ", lb_value, ") & (", element, " >= ", value, ")")
    )
  
    spell_day <- instatCalculations::instat_calculation$new(
        type="calculation", 
        function_exp=fn_exps, 
        result_name="spell_day", 
        calculated_from= setNames(list(element), data))
    
    spell_length <- instatCalculations::instat_calculation$new(
        type="calculation", 
        function_exp="instatClimatic::spells(x=spell_day)",
        result_name="spell_length", 
        sub_calculations=list(spell_day), 
        save=0)
  
    
    # build the grouping vars
    group_vars <- c(station, year) %>% stats::na.omit()
    calc_from <- setNames(
        as.list(group_vars), 
        rep(data, length(group_vars))
    )
    
    grouping <- instatCalculations::instat_calculation$new(
        type="by", 
        calculated_from=calc_from)
  
    # Checking if the column names for start and end rains were given, else use the numeric values (1 to 366).
    if (!is.null(start_rain) & !is.null(end_rain)){
        fn_day_exps <- paste0(doy, " >= ", start_date, " & ", doy, " <= ", end_date)
    }
    else {
        fn_day_exps <- paste0(doy, " >= ", day_from, " & ", doy, " <= ", day_to)
    }
    day_from_and_to <- instatCalculations::instat_calculation$new(
        type="filter", 
        function_exp=fn_day_exps, 
        calculated_from=databook::calc_from_convert(x=setNames(list("doy", c(start_date, end_date)), c(data, summary_data))))
    
    spells <- instatCalculations::instat_calculation$new(
        type="summary", 
        function_exp="max(x=spell_length)", 
        result_name="spells", 
        manipulations=list(spell_length, grouping, day_from_and_to), 
        save=2)
    
    data_book$run_instat_calculation(calc=spells, display=FALSE)
    
    # This is the bit that then is different:
    spells_filter <- instatCalculations::instat_calculation$new(
        type="filter", 
        function_exp="dplyr::lead(c(NA,diff(spell_length)))<0", 
        sub_calculations=list(spell_length), 
        save=2, 
        result_data_frame="spells_filter")
    
    data_book$run_instat_calculation(calc=spells_filter, display=FALSE)
  
}