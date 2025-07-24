#' Shift Dates Function
#'
#' This function shifts dates in a data frame to a specified day of the year (DOY) and calculates the corresponding year shift.
#'
#' @param data A data frame containing date information.
#' @param date The name of the date column in the data frame.
#' @param s_start_doy The starting day of the year (DOY) for the shift.
#' @param doy_shift_name (Optional) The name for the new DOY shift column in the data frame (default is "s_doy").
#' @param year_shift_name (Optional) The name for the new year shift column in the data frame (default is "s_year").
#'
#' @return The input data frame with additional columns for DOY shift and year shift.
#'
#' @export
#'
#' @examples
#' data <- data.frame(date = as.Date(c("2023-03-15", "2024-02-29")), year = c(2023, 2024))
#' shifted_data <- shift_dates(data, "date", 50, "shifted_doy", "shifted_year")
shift_dates <- function(data, date, s_start_doy, doy_shift_name = "s_doy", year_shift_name = "s_year"){
  # Calculate day of the year for leap years (366 days)
  doy <- instatExtras::yday_366(data[[date]])
  
  # Calculate the DOY shift
  doy_shift <- (doy - s_start_doy) %% 366
  
  # Handle DOY 366
  doy_shift <- ifelse(doy_shift == 0, 366, doy_shift)
  
  # Add the DOY shift and year shift columns to the data frame
  data[[doy_shift_name]] <- doy_shift
  data[[year_shift_name]] <- ifelse(data[[doy_shift_name]] < s_start_doy,
                                    paste0(data[["year"]], "-", data[["year"]] + 1),
                                    paste0(data[["year"]] - 1, "-", data[["year"]]))
  
  # Return the modified data frame
  return(data)
}