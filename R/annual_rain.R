#' Annual rainfall summaries
#'
#' @param data 
#' @param date_time 
#' @param rain 
#' @param year 
#' @param total_rain 
#' @param n_rain 
#' @param rain_day 
#' @param station 
#' @param na_rm 
#' @param na_prop 
#' @param na_n 
#' @param na_consec 
#' @param na_n_non 
#'
#' @return A data.frame with rainfall summaries for each year
#' @export
#'
#' @examples #TODO
annual_rain <- function(data, date_time, rain, year, total_rain = TRUE,
                        n_rain = TRUE, rain_day = 0.85, station = NULL, 
                        na_rm = FALSE, na_prop = NULL, na_n = NULL, 
                        na_consec = NULL, na_n_non = NULL) {
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