#' Join Null Data
#'
#' This function joins two data frames, `summary_data` and `calculated_data`, 
#' using a full join if `summary_data` is not NULL. If `summary_data` is NULL,
#' it assigns `calculated_data` to `summary_data`.
#'
#' @param summary_data A data frame representing summary data.
#' @param calculated_data A data frame containing calculated data.
#'
#' @return A data frame resulting from the full join of `summary_data` and `calculated_data`,
#' or `calculated_data` if `summary_data` is NULL.
#'
#' @export
#'
#' @examples
#' # summary_data is NULL
#' summary_data <- NULL
#' calculated_data <- data.frame(x = 1:5, y = letters[1:5])
#' join_null_data(summary_data, calculated_data)
#'
#' # summary_data is not NULL
#' summary_data <- data.frame(x = 1:3, y = letters[1:3])
#' calculated_data <- data.frame(x = 4:5, y = letters[4:5])
#' join_null_data(summary_data, calculated_data)
join_null_data <- function(summary_data, calculated_data){
  if (is.null(summary_data)){
    summary_data <- calculated_data
  } else{
    summary_data <- dplyr::full_join(summary_data, calculated_data)
  }
  return(summary_data)
}
