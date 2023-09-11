#' Calculate summaries from climatic data
#' 
#' @description \code{climatic_summary} returns a data table displaying
#' summary statistics for element(s) (and for each station) in a given time period.
#' 
#' @param data \code{data.frame} The data.frame to calculate from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param elements \code{character} The name of the elements column in \code{data} to apply the function to.
#' @param year \code{character(1)} The name of the year column in \code{data}.
#' @param month \code{character(1)} The name of the month column in \code{data}. 
#' @param dekad \code{character(1)} The name of the dekad column in \code{data}.
#' @param pentad \code{character(1)} The name of the pentad column in \code{data}.
#' @param to \code{character(1)} The date-time format to put the data into.
#' @param by \code{character} The name of columns in \code{data} to group the summary data by.
#' @param doy \code{character(1)} The name of the day of the year (1-366) column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[doy]])}.
#' @param doy_first \code{integer(1)} The first day of the year.
#' @param doy_last \code{integer(1)} The last day of the year.
#' @param summaries \code{character} A named character vector of summary functions. The names are
#'   used as the column names in the results. The values can be any function
#'   name as a string or a function call as a formula. e.g. c(mean = "mean", st_dv = "sd", n = "~dplyr::n()")
#' @param na_rm \code{logical(1)} If \code{TRUE} all \code{na_} parameters are ignored and missing
#'   values are removed. If \code{FALSE} missing values are not removed unless
#'   any \code{na_} parameters are specified.
#' @param na_prop \code{integer(1)} Max proportion of missing values allowed
#' @param na_n \code{integer(1)} Max number of missing values allowed
#' @param na_consec \code{integer(1)} Max number of consecutive missing values allowed
#' @param na_n_non \code{integer(1)} Min number of non-missing values required
#' @param names Format of column names. Passed to \code{.names} in
#'   \code{dplyr::across}
#' @param summaries_params \code{list} Additional parameters to pass to \code{summaries}.
#'   Must be a list of lists with the same names as \code{summaries}.
#' @param first_date \code{logical(1)} If \code{TRUE} the first instance of \code{date_time} when the value equals the summary value is included. Generally only used for extreme summaries i.e. first \code{date_time} when the maximum occurred.
#' @param n_dates \code{logical(1)} If \code{TRUE} the number of \code{date_time} points when the value equals the summary value is included. Generally only used for extreme summaries i.e. number of days in which the minimum occurred.
#' @param last_date \code{logical(1)} If \code{TRUE} the last instance of \code{date_time} when the value equals the summary value is included. Generally only used for extreme summaries i.e. last \code{date_time} when the maximum occurred.
#'
#' @return A summary data frame for selected element(s) in climatic data.
#' @export
#'
#' @examples
#' # Calculate frequencies for tmin/tmax for each year, month, and station.
#' # filter daily_niger data for the example
#' daily_niger_1 <- daily_niger %>% dplyr::filter(year > 1960)
#' climatic_summary(data = daily_niger_1, date_time = "date", station = "station_name",
#'                  elements = c("tmin", "tmax"), to = "monthly")
#' @importFrom rlang .data

climatic_summary <- function(data, date_time, station = NULL, elements = NULL, 
                             year = NULL, month = NULL, dekad = NULL, 
                             pentad = NULL,
                             to = c("hourly", "daily", "pentad", "dekadal", 
                                    "monthly", "annual-within-year", 
                                    "annual", "longterm-monthly", 
                                    "longterm-within-year", "station",
                                    "overall"),
                             by = NULL,
                             doy = NULL, doy_first = 1, doy_last = 366, 
                             summaries = c(n = "~dplyr::n()"), na_rm = FALSE,
                             na_prop = NULL, na_n = NULL, na_consec = NULL, 
                             na_n_non = NULL,
                             first_date = FALSE, n_dates = FALSE, last_date = FALSE,
                             summaries_params = list(), names = "{.fn}_{.col}") {
  
  checkmate::assert_data_frame(data)
  assert_column_names(data, date_time)
  checkmate::assert(checkmate::check_date(data[[date_time]]), 
                    checkmate::check_posixct(data[[date_time]]))
  checkmate::assert_character(elements)
  assert_column_names(data, elements)
  if (length(elements) == 1 && 
      elements == "obsValue" && 
      "describedBy" %in% names(data)) {
    element_names <- as.character(unique(data[["describedBy"]]))
    data <- elements_wider(data, name = "describedBy", value = elements)
    elements <- element_names
  }
  checkmate::assert_string(station, null.ok = TRUE)
  if (!is.null(station)) assert_column_names(data, station)
  checkmate::assert_string(year, null.ok = TRUE)
  if (!is.null(year)) assert_column_names(data, year)
  checkmate::assert_string(dekad, null.ok = TRUE)
  if (!is.null(dekad)) assert_column_names(data, dekad)
  checkmate::assert_string(pentad, null.ok = TRUE)
  if (!is.null(dekad)) assert_column_names(data, pentad)
  if (!is.null(by)) assert_column_names(data, by)
  
  to <- match.arg(to)
  checkmate::assert_character(summaries)
  
  checkmate::assert_logical(na_rm)
  checkmate::assert_number(na_prop, lower = 0, upper = 1, null.ok = TRUE)
  checkmate::assert_int(na_n, lower = 0, null.ok = TRUE)
  checkmate::assert_int(na_consec, lower = 0, null.ok = TRUE)
  checkmate::assert_int(na_n_non, lower = 0, null.ok = TRUE)
  
  checkmate::assert_logical(first_date)
  checkmate::assert_logical(n_dates)
  checkmate::assert_logical(last_date)
  
  any_na_params <- !is.null(na_prop) || !is.null(na_n) || 
    !is.null(na_consec) || !is.null(na_n_non)
  if (na_rm) {
    if (any_na_params) {
      warning("'na_rm = TRUE' will override na_prop, 
            na_n, na_consec and na_n_non")
    }
    na_prop <- 1
    na_n <- NULL
    na_consec <- NULL
    na_n_non <- NULL
    any_na_params <- TRUE
  }
  checkmate::assert_list(summaries_params, types = "list", names = "unique")
  
  checkmate::assert_int(doy_first, lower = 1, upper = 366)
  checkmate::assert_int(doy_last, lower = 1, upper = 366)
  if (doy_first > doy_last) 
    stop("doy_first must be less than or equal to doy_last")
  if (doy_first > 1 || doy_last < 366) {
    if (is.null(doy)) {
      doy <- "doy"
      data[[doy]] <- yday_366(data[[date_time]])
    }
  }
  
  if (to == "station" && is.null(station)) {
    stop("station is required when to = 'station'")
  }
  
  # Check year column exists or create
  if (to %in% c("pentad", "dekadal", "monthly", "annual", 
                "annual-within-year")) {
    if (is.null(year)) {
      year <- "year"
      data[[year]] <- lubridate::year(data[[date_time]])
    }
  }
  
  if (to %in% c("monthly", "longterm-monthly")) {
    if (is.null(month)) {
      month <- "month"
      data[[month]] <- lubridate::month(data[[date_time]])
    }
  }
  
  # to = "pentad" "dekadal" "monthly" or "longterm-monthly"
  # are special cases of "within" cases and can be converted
  # to simplify case handling internally.
  if (to == "pentad") {
    if (is.null(pentad)) {
      pentad <- "pentad"
      data[[pentad]] <- pentad(data[[date_time]])
    }
    to <- "annual-within-year"
    by <- pentad
  } else if (to == "dekadal") {
    if (is.null(pentad)) {
      dekad <- "dekad"
      data[[dekad]] <- dekad(data[[date_time]])
    }
    to <- "annual-within-year"
    by <- dekad
  } else if (to == "monthly") {
    to <- "annual-within-year"
    by <- month
  } else if (to == "longterm-monthly") {
    to <- "longterm-within-year"
    by <- month
  } 
  
  if (!is.null(station) && to != "overall") {
    grp_data <- data %>%
      dplyr::group_by(.data[[station]])
  } else grp_data <- data
  if (to == "hourly") {
    hour <- "hour"
    grp_data[[hour]] <- lubridate::hour(grp_data[[date_time]])
    date <- ".date"
    grp_data[[date]] <- as.Date(grp_data[[date_time]])
    grp_data <- grp_data %>%
      dplyr::group_by(.data[[date]], .data[[hour]], .add = TRUE)
  } else if (to == "daily") {
    date <- "date"
    grp_data[[date]] <- as.Date(grp_data[[date_time]])
    grp_data <- grp_data %>%
      dplyr::group_by(.data[[date]])
  } else if (to == "annual-within-year") {
    if (is.null(by)) stop("by is required when to = 'annual-within-year'")
    grp_data <- grp_data %>%
      dplyr::group_by(.data[[year]], .add = TRUE)
    for (i in seq_along(by)) {
      grp_data <- grp_data %>%
        dplyr::group_by(.data[[by[i]]], .add = TRUE)
    }
  } else if (to == "annual") {
    grp_data <- grp_data %>%
      dplyr::group_by(.data[[year]], .add = TRUE)
  } else if (to == "longterm-within-year") {
    if (is.null(by)) stop("'by' is required when to = 'annual-within-year'")
    for (i in seq_along(by)) {
      grp_data <- grp_data %>%
        dplyr::group_by(.data[[by[i]]], .add = TRUE)
    }
  }
  if (doy_first > 1 || doy_last < 366) {
    grp_data <- grp_data %>%
      dplyr::filter(.data[[doy]] >= doy_first & .data[[doy]] <= doy_last, .preserve = TRUE)
  }
  if (!any_na_params) .x_call <- ".x"
  else {
    # na_params <- c(null_to_string(na_prop), null_to_string(na_n),
    #                null_to_string(na_consec), null_to_string(na_n_non))
    # .x_call <- paste0("naflex::na_omit_if(.x, ",
    #                   "prop = ", na_params[1], ", ", 
    #                   "n = ", na_params[2], ", ", 
    #                   "consec = ", na_params[3], ", ",
    #                   "n_non = ", na_params[4], ")")
  }
  exp_summaries <- vector("list", length(summaries))
  names(exp_summaries) <- names(summaries)
  lambda_summaries <- exp_summaries
  for (i in seq_along(summaries)) {
    fn_exp <- summaries[[i]]
    if (!startsWith(fn_exp, "~")) {
      fn_exp <- paste0(fn_exp, "(", .x_call)
      add_params <- summaries_params[[names(summaries)[i]]]
      fn_exp <- add_params(fn_exp, add_params)
      fn_exp <- paste0(fn_exp, ")")
    }
    exp_summaries[[i]] <- fn_exp
    lambda_summaries[[i]] <- ifelse(startsWith(fn_exp, "~"), fn_exp, 
                                    paste0("~", fn_exp))
  }
  lambda_summaries <- sapply(lambda_summaries, stats::formula)
  names(lambda_summaries) <- names(summaries)
  sum_data <- grp_data %>%
    dplyr::summarise(dplyr::across(tidyselect::all_of(elements), 
                                   lambda_summaries, .names = names))
  
  if (first_date || n_dates || last_date) {
    date_summaries <- list()
    if (first_date) date_summaries[["first"]] <- dplyr::first
    # n() needs to be called like this 
    #  otherwise the column with be passed to n() giving and error
    if (n_dates) date_summaries[["n"]] <- ~ dplyr::n()
    if (last_date) date_summaries[["last"]] = dplyr::last
    if (length(summaries) == 1 && length(elements) == 1) adjust_names <- FALSE
    else adjust_names <- TRUE
    k <- 1
    date_summaries_data_list <- vector("list", length(summaries) * length(elements))
    for (i in seq_along(elements)) {
      for (j in seq_along(summaries)) {
        exp_summaries_elements <- gsub(".x", elements[i], exp_summaries,
                                       fixed = TRUE)
        fil_data <- grp_data %>%
          dplyr::filter(`==`(.data[[elements[i]]], 
                             !! rlang::parse_expr(exp_summaries_elements[j])),
                        .preserve = TRUE)
        date_summaries_data <- fil_data %>%
          dplyr::summarise(
            dplyr::across(dplyr::all_of(date_time), date_summaries,
                          .names = ifelse(adjust_names, 
                                          paste(names, 
                                                summaries[j], 
                                                elements[i],
                                                sep = "_"),
                                          names)))
        date_summaries_data_list[[k]] <- date_summaries_data
        k <- k + 1
      }
    }
    join_grps <- as.character(dplyr::groups(grp_data))
    extra_date_summaries <- 
      Reduce(function(x, y) dplyr::left_join(x, y, 
                                             by = join_grps),
             date_summaries_data_list
      )
    sum_data <- dplyr::left_join(sum_data, extra_date_summaries, 
                                 by = join_grps)
  }
  return(sum_data)
}
