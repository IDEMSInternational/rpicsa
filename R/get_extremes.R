#' Get Extreme Data
#' 
#' This function identifies extreme values in a specified element (column) of a data frame. It can operate in two modes: percentile-based and threshold-based.
#' 
#' @param data A data frame containing the data to be analysed.
#' @param element The name of the column in 'data' for which extremes are to be found.
#' @param type A character string specifying the mode of operation. It can be either `"percentile"` or `"threshold"`. Here, `"percentile"` identifies values above a certain percentile (e.g., 95th percentile); `"threshold"` identifies values above a specific threshold value.
#' @param value A numeric value specifying the percentile or threshold, depending on the 'type' parameter. If `type == "percentile"`, `value` is the percentile (e.g., 95 for 95th percentile). If `type == "threshold"`, `value` is the threshold value (e.g., 50 mm for rainfall).
#' @param direction A character string specifying the direction for the operation. It can be either `"greater"` or `"less"`.
#' 
#' @export
#' @return A filtered data frame where the `element` values are considered extreme based on the specified `type` and `value`.
#' 
#' @examples
#' # data(daily_niger)
#' # filtered_data <- get_extremes(data = daily_niger, element = "rain", type = "threshold", value = 50)
get_extremes <- function(data, element, type = c("percentile", "threshold"), value = 95, direction = c("greater", "less")) {
  type <- match.arg(type)
  direction <- match.arg(direction)
  
  # Check if element exists in data
  if (!element %in% names(data)) {
    stop("Element column not found in the data frame.")
  }
  
  # Determine the threshold value based on the specified type
  if (type == "percentile") {
    threshold_value <- quantile(data[[element]], probs = value/100, na.rm = TRUE)
  } else {
    threshold_value <- value
  }
  
  # Filter data based on the threshold and direction
  if (direction == "greater") {
    extreme_data <- data %>% dplyr::filter(.data[[element]] > threshold_value)
  } else {
    extreme_data <- data %>% dplyr::filter(.data[[element]] < threshold_value)
  }
  
  return(extreme_data)
}