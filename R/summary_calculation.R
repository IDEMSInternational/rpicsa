#' Summary Calculation
#' 
#' @description Returns a summary data frame giving either the mean, minimum, and/or maximum temperatures by year and/or month, and by optionally station.
#' 
#' @param data The data.frame to calculate from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param columns_to_summarise \code{character(1)} The name of the columns to summarise in \code{data}.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param month \code{character(1)} The name of the month column in \code{data}. If \code{NULL} it will be created using \code{lubridate::month(data[[date_time]])}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param to \code{character(1)} Default `annual`. The period of time to calculate the mean temperature columns over (options are `annual` or `monthly`).
#' @param summaries \code{character} The summaries to display. Options are `"mean"`, `"max"`, `"min"`, `"sum"`
#' @param na_rm \code{logical(1)}. Should missing values (including \code{NaN}) be removed?
#' @param na_prop \code{integer(1)} Max proportion of missing values allowed
#' @param na_n \code{integer(1)} Max number of missing values allowed
#' @param na_consec \code{integer(1)} Max number of consecutive missing values allowed
#' @param na_n_non \code{integer(1)} Min number of non-missing values required
#' @param data_book The data book object where the data object is stored, default `NULL`.
#'
#' @return A data.frame with summaries for the columns_to_summarise specified by year and/or month (and optionally station)
#' @export
summary_calculation <- function(data, date_time, year = NULL, month = NULL,
                                station = NULL, to = c("annual", "monthly"),
                                columns_to_summarise,
                                summaries = c("mean", "min", "max", "sum"), na_rm = FALSE,
                                na_prop = NULL, na_n = NULL, na_consec = NULL, na_n_non = NULL,
                                data_book = NULL){
  
  if (is.null(data_book)) {
    data_book <- DataBook$new()
  }
  
  # Running checks
  checkmate::assert_string(data)
  checkmate::assert_string(date_time)
  checkmate::assert_string(year, null.ok = TRUE)
  checkmate::assert_string(month, null.ok = TRUE)
  checkmate::assert_string(station, null.ok = TRUE)
  data_frame <- data_book$get_data_frame(data)
  assert_column_names(data_frame, date_time)
  checkmate::assert(checkmate::check_date(data_frame[[date_time]], null.ok = TRUE), 
                    checkmate::check_posixct(data_frame[[date_time]],  null.ok = TRUE))
  if (!is.null(year)) assert_column_names(data_frame, year)
  if (!is.null(month)) assert_column_names(data_frame, month)
  if (!is.null(station)) assert_column_names(data_frame, station)
  if (!is.null(rain)) assert_column_names(data_frame, rain)
  checkmate::assert_logical(na_rm, null.ok = TRUE)
  checkmate::assert_int(na_prop, null.ok = TRUE)
  checkmate::assert_int(na_n, null.ok = TRUE)
  checkmate::assert_int(na_consec, null.ok = TRUE)
  checkmate::assert_int(na_n_non, null.ok = TRUE)
  to <- match.arg(to)
  
  # creating the year and month columns if they do not exist
  if (is.null(year)) {
    data_book$split_date(data_name=data, col_name=date_time, year_val=TRUE, s_start_month=1)
    year <- "year"
  }
  
  if (to == "monthly" && is.null(month)) {
    data_book$split_date(data_name=data, col_name=date_time, month_val=TRUE, s_start_month=1)
    month <- "month"
  }
  
  # creating the grouping list
  grouping_vars <- c()
  if (!is.null(station)) { grouping_vars <- c(grouping_vars, station) }
  if (to == "annual") { grouping_vars <- c(grouping_vars, year) }
  if (to == "monthly") { grouping_vars <- c(grouping_vars, month) }
  
  # creating the summaries list
  summaries_all <- c()
  if ("mean" %in% summaries){ summaries_all <- c(summaries_all, mean = "mean")}
  if ("min" %in% summaries){ summaries_all <- c(summaries_all, min = "min")}
  if ("max" %in% summaries){ summaries_all <- c(summaries_all, max = "max")}
  if ("sum" %in% summaries){ summaries_all <- c(summaries_all, max = "sum")}
  summaries_all <- paste0("summary_", summaries_all)
  
  # Defining NA types
  na_type <- c(
    if (!is.null(na_n))        "n",
    if (!is.null(na_n_non))    "n_non_miss",
    if (!is.null(na_prop))     "prop",
    if (!is.null(na_consec))   "con"
  )
  
  data_book$calculate_summary(data_name = data,
                              columns_to_summarise = columns_to_summarise,
                              factors = grouping_vars,
                              store_results = TRUE,
                              return_output = FALSE,
                              summaries = summaries_all,
                              silent = TRUE,
                              na.rm = na_rm,
                              na_type = na_type, 
                              na_max_n = na_n,
                              na_min_n = na_n_non,
                              na_consecutive_n = na_consec,
                              na_max_prop = na_prop)
  
}