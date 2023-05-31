#' Annual total rainfall
#' @description Returns a summary data frame giving the total rainfall each year from 1 Jan to 31 Dec.
#' 
#' @param data The data.frame to calculate from.
#' @param date_time \code{\link[base]{Date}} The name of the date column in \code{data}.
#' @param rain \code{character(1)} The name of the rainfall column in \code{data} to apply the function to.
#' @param year \code{character(1)} The name of the year column in \code{data}. If \code{NULL} it will be created using \code{lubridate::year(data[[date_time]])}.
#' @param station \code{character(1)} The name of the station column in \code{data}, if the data are for multiple station.
#' @param total_rain \code{logical(1)} default `TRUE`. Display the total rainfall value for each year.
#' @param n_rain \code{logical(1)} default `TRUE`. Display the number of rainfall days.
#' @param rain_day \code{numerical(1)} If `n_rain = TRUE`, the minimum rainfall value in a day for that day to count as a rainfall day.
#' @param na_rm \code{logical(1)}. Should missing values (including \code{NaN}) be removed?
#' @param na_prop \code{integer(1)} Max proportion of missing values allowed
#' @param na_n \code{integer(1)} Max number of missing values allowed
#' @param na_consec \code{integer(1)} Max number of consecutive missing values allowed
#' @param na_n_non \code{integer(1)} Min number of non-missing values required
#'
#' @return A data.frame with rainfall summaries for each year
#' @export
#'
#' @examples #daily_niger_1 <- daily_niger %>% dplyr::filter(year > 1960)
#' #annual_rain(data = daily_niger, date_time  = "date", station = "station_name",
#' #            rain = "rain", na_prop = 0.9)

annual_rain <- function(data, date_time, rain, year = NULL, total_rain = TRUE,
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