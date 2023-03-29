#' Seasonal total rainfall
#' @description Total annual rainfall between start of the rains and end of the season.
#' 
#' @inheritParams annual_rain
#' 
#' @param summary_data Summary data frame containing the `start_date` and `end_date` variables. These variables are calculated from start of rains and end of season functions.
#' If `NULL`, `start_date` and `end_date` are calculated from the `start_of_rains` and `end_of_season` functions respectively.
#' @param start_date \code{character(1)} The name of the start of rains column in \code{summary_data}. If \code{NULL} it will be created using the \code{start_of_rains} function.
#' @param end_date \code{character(1)} The name of the end of season column in \code{summary_data}. If \code{NULL} it will be created using the \code{end_of_seasons} function.
#'
#' @return A data.frame with rainfall summaries for each year in the specified season (between start of the rains and end of season).
#' @export
#'
#' @examples # TODO
seasonal_rain <- function(data, date_time, rain, year = NULL, station = NULL, 
                          summary_data = NULL, start_date = NULL, end_date = NULL,
                          total_rain = TRUE, n_rain = TRUE, rain_day = 0.85,
                          na_rm = FALSE, na_prop = NULL, na_n = NULL, 
                          na_consec = NULL, na_n_non = NULL) {
  
  # if summary_data is NULL, we calculate the SOR/EOR
  # else ...
  ## if start date is NULL, calculate it
  ## if end date is NULL, calculate it
  
  # then merge the start/end date columns into the data frame?
  data <- dplyr::full_join(data, summary_data)
  # now we have start_date and end_date in the data.
  
  # have to check if start_date and end_date are dates or doy. 
  # if doy, convert?
  
  # filter data >= start and data <= end
  
  if (!total_rain && !n_rain) {
    stop("No summaries selected. At least one of
         'total_rain' or 'n_rain' must be TRUE.")
  }
  summaries <- c()
  if (total_rain) summaries <- c(total_rain = "sum")
  if (n_rain) summaries <- c(summaries, 
                             n_rain = paste0("~sum(.x > ", rain_day, ")"))
  cdms.products::climatic_summary(data = data, date_time = date_time, 
                                  station = station, elements = rain,
                                  year = year, to = "annual", 
                                  summaries = summaries, na_rm = na_rm, 
                                  na_prop = na_prop, na_n = na_n, 
                                  na_n_non = na_n_non, names = "{.fn}")
}