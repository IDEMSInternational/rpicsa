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
#'
#' @return A data.frame with mean summaries for each year or year and month for the minimum daily temperature and/or the maximum daily temperature.
#' @export
#'
#' @examples #daily_niger_1 <- daily_niger %>% filter(year < 1950)
#' #summary_temperature(data = daily_niger_1, date_time  = "date", station = "station_name",
#' #             tmax = "tmax", tmin = "tmin", na_prop = 0.05)

summary_temperature <- function(data, date_time, tmin = NULL, tmax = NULL, year = NULL,
                             month = NULL, station = NULL, to = c("annual", "monthly"),
                             summaries = c("mean", "min", "max"), na_rm = FALSE,
                             na_prop = NULL, na_n = NULL, na_consec = NULL, na_n_non = NULL) {
  to <- match.arg(to)
  summaries_all <- c()
  if ("mean" %in% summaries){ summaries_all <- cbind(summaries_all, mean = "mean")}
  if ("min" %in% summaries){ summaries_all <- cbind(summaries_all, min = "min")}
  if ("max" %in% summaries){ summaries_all <- cbind(summaries_all, max = "max")}
  climatic_summary(data = data, date_time = date_time, 
                                  station = station, elements = c(tmin, tmax),
                                  year = year, month = month, to = to, 
                                  summaries = c(mean = "mean"), na_rm = na_rm, 
                                  na_prop = na_prop, na_n = na_n, 
                                  na_n_non = na_n_non, names = "{.fn}_{.col}")
}
