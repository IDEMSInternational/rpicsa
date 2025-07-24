#' Summary Temperature (Month or annually)
#' @description Returns a summary data frame giving either the mean, minimum, and/or maximum temperatures each year from 1 Jan to 31 Dec, or by year and month.
#' 
#' @param data The data.frame to calculate from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param tmin \code{character(1)} The name of the minimum temperature column in \code{data} to apply the function to.
#' @param tmax \code{character(1)} The name of the maximum temperature column in \code{data} to apply the function to.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param month \code{character(1)} The name of the month column in \code{data}. If \code{NULL} it will be created using \code{lubridate::month(data[[date_time]])}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param to \code{character(1)} Default `annual`. The period of time to calculate the mean temperature columns over (options are `annual` or `monthly`).
#' @param summaries \code{character} The summaries to display. Options are `"mean"`, `"max"`, `"min"`.
#' @param na_rm \code{logical(1)}. Should missing values (including \code{NaN}) be removed?
#' @param na_prop \code{integer(1)} Max proportion of missing values allowed
#' @param na_n \code{integer(1)} Max number of missing values allowed
#' @param na_consec \code{integer(1)} Max number of consecutive missing values allowed
#' @param na_n_non \code{integer(1)} Min number of non-missing values required
#' @param data_book The data book object where the data object is stored, default `NULL`.
#'
#' @return A data.frame with mean summaries for each year or year and month for the minimum daily temperature and/or the maximum daily temperature.
#' @export
#'

summary_temperature <- function(data, date_time, tmin = NULL, tmax = NULL, year = NULL,
                                month = NULL, station = NULL, to = c("annual", "monthly"),
                                summaries = c("mean", "min", "max"), na_rm = FALSE,
                                na_prop = NULL, na_n = NULL, na_consec = NULL, na_n_non = NULL,
                                data_book = NULL) {
  if (is.null(data_book)) {
    data_book <- DataBook$new()
  }
  
  to <- match.arg(to)
  if (is.null(tmin) && is.null(tmax)) { stop("At least one of 'tmin' or 'tmax' must be provided.") }
  
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
  
  # specifying the columns to summarize
  columns_to_summarize <- c(tmin, tmax)
  columns_to_summarize <- columns_to_summarize[!sapply(columns_to_summarize, is.null)]
  
  # creating the summaries list
  summaries_all <- c()
  if ("mean" %in% summaries){ summaries_all <- c(summaries_all, mean = "mean")}
  if ("min" %in% summaries){ summaries_all <- c(summaries_all, min = "min")}
  if ("max" %in% summaries){ summaries_all <- c(summaries_all, max = "max")}
  summaries_all <- paste0("summary_", summaries_all)
  
  na_type <- c(
    if (!is.null(na_n))        "n",
    if (!is.null(na_n_non))    "n_non_miss",
    if (!is.null(na_prop))     "prop",
    if (!is.null(na_consec))   "con"
  )
  
  data_book$calculate_summary(data_name = data,
                              columns_to_summarise = columns_to_summarize,
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