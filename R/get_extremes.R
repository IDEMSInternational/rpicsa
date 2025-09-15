#' Get Extreme Data
#' 
#' This function identifies extreme values in a specified element (column) of a data frame. It can operate in two modes: percentile-based and threshold-based.
#' 
#' @param data A data frame containing the data to be analysed.
#' @param date_time \code{character(1)} Column name (in \code{data}) of the date variable.
#' @param year The name of the `year` column in 'data'.
#' @param element The name of the column in 'data' for which extremes are to be found.
#' @param station The name of the `station` column in 'data'. Default `NULL`.
#' @param type A character string specifying the mode of operation. It can be either `"percentile"` or `"threshold"`. Here, `"percentile"` identifies values above a certain percentile (e.g., 95th percentile); `"threshold"` identifies values above a specific threshold value.
#' @param value A numeric value specifying the percentile or threshold, depending on the 'type' parameter. If `type == "percentile"`, `value` is the percentile (e.g., 95 for 95th percentile). If `type == "threshold"`, `value` is the threshold value (e.g., 50 mm for rainfall).
#' @param direction A character string specifying the direction for the operation. It can be either `"greater"` or `"less"`.
#' @param lb_value A numeric value for the lower bound if `direction == "between"` or `direction == "outer"`
#' @param na_rm \code{logical(1)} Should missing values be removed before summing? Passed through to
#'   the summary calculation. Default \code{FALSE}.
#' @param na_prop \code{numeric(1)} Maximum allowed proportion of missing values in the in-season window.
#' @param na_n \code{integer(1)} Maximum allowed number of missing values.
#' @param na_consec \code{integer(1)} Maximum allowed number of consecutive missing values.
#' @param na_n_non \code{integer(1)} Minimum required count of non-missing values.
#' @param data_book The \code{DataBook} (R6) object holding \code{data} and \code{summary_data}.
#' 
#' @export
#' @return A filtered data frame where the `element` values are considered extreme based on the specified `type` and `value`.

get_extremes <- function(data, element, date_time=NULL, year=NULL, station = NULL, type = c("percentile", "threshold"), 
                         value = 95, direction = c("greater", "less", "between", "outer"), lb_value = 0, 
                         na_rm = FALSE, na_prop = NULL, na_n = NULL, na_consec = NULL, na_n_non = NULL,  
                         data_book = NULL) {
    
    if (is.null(data_book)){
      data_book = DataBook$new()
    }
  
    # Running checks for data
    checkmate::assert_string(data)
    checkmate::assert_string(element)
    checkmate::assert_string(date_time, null.ok=TRUE)
    checkmate::assert_string(year, null.ok = TRUE)
    checkmate::assert_string(station, null.ok = TRUE)
    checkmate::assert_numeric(value)
    checkmate::assert_numeric(lb_value)
    
    data_frame <- data_book$get_data_frame(data)
    if (!is.null(date_time)) assert_column_names(data_frame, date_time)
    if (!is.null(year)) assert_column_names(data_frame, year)
    if (!is.null(station)) assert_column_names(data_frame, station)

    checkmate::assert_logical(na_rm, null.ok = TRUE)
    checkmate::assert_int(na_prop, null.ok = TRUE)
    checkmate::assert_int(na_n, null.ok = TRUE)
    checkmate::assert_int(na_consec, null.ok = TRUE)
    checkmate::assert_int(na_n_non, null.ok = TRUE)
  
    type <- match.arg(type)
    direction <- match.arg(direction)
    
    if (is.null(year)) {
        data_book$split_date(data_name = data_name, col_name = date, year_val = TRUE, s_start_month = 1)
        year <- "year"
    }
    
    if (direction == "greater"){
        fn_exps = paste0("(", element, " >= ", value, ")")
    }
    else if (direction == "less"){
        fn_exps = paste0("(", element, " <= ", value, ")")
    }
    else if (direction == "between"){
        fn_exps = paste0("(", element, " >= ", lb_value, ") & (", element, " <= ", value, ")")
    } 
    else if (direction == "outer"){
        fn_exps = paste0("(", element, " <= ", lb_value, ") & (", element, " >= ", value, ")")
    }
    
    rain_day <- instatCalculations::instat_calculation$new(
        type="calculation", 
        function_exp=fn_exps, 
        result_name="rain_day", 
        calculated_from=setNames(list(element), data))
    
    # Then we give a 0/1 depending if it is in that bracket or not
    transform_calculation <- instatCalculations::instat_calculation$new(
        type="calculation", 
        function_exp="zoo::rollapply(data=rain_day, width=1, FUN=sum, align='right', fill=NA)", 
        result_name="count", 
        sub_calculations=list(rain_day), 
        save=2, 
        before=FALSE, 
        adjacent_column=element)
    data_book$run_instat_calculation(calc=transform_calculation, display=FALSE)
    
    na_type <- c(
        if (!is.null(na_n))        "n",
        if (!is.null(na_n_non))    "n_non_miss",
        if (!is.null(na_prop))     "prop",
        if (!is.null(na_consec))   "con"
    )
    
    summary_calculation(data = data,
                        date_time = date_time,
                        station = station,
                        year = year,
                        to = "annual",
                        columns_to_summarise = count, # count is the name of our created column in the code above
                        summaries = "sum",
                        na_rm = na_rm,
                        na_prop = na_prop,
                        na_n = na_n,
                        na_consec = na_consec,
                        na_n_non = na_n_non,
                        data_book = data_book)
  
}
