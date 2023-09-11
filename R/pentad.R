#' Get the pentad component of a date-time object
#'
#' @description Convert a date or date-time object to a yearly pentad (5-day period)
#' 
#' @param date A date-time object
#'
#' @return a numerical vector of pentad objects corresponding to date variable.
#' @export
#'
#' @examples
#' pentad(as.Date("2020/12/25"))
#' pentad(as.Date("1999/01/01"))
pentad <- function(date) {
  temp_pentad <- 6 * (lubridate::month(date)) - 5 + (lubridate::mday(date) > 5) + (lubridate::mday(date) > 10) + (lubridate::mday(date) > 15) + (lubridate::mday(date) > 20) + (lubridate::mday(date) > 25)
  return(temp_pentad)	
}	