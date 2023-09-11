#' Replace NULL Values with "NULL" in R
#'
#' This function takes an R object and checks if it is NULL. If the input object 
#' is NULL, it returns the string "NULL"; otherwise, it returns the input object 
#' itself. This is useful for converting NULL values to a string representation 
#' when needed.
#'
#' @param x An R object that you want to check for NULL.
#'
#' @return If 'x' is NULL, the function returns the string "NULL"; otherwise, it 
#'         returns 'x' itself.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' 
#' # Create a NULL object
#' null_object <- NULL
#'
#' # Convert NULL to "NULL"
#' result <- null_to_string(null_object)
#' result
#'
#' # Output:
#' # [1] "NULL"
#'
#' # Try with a non-NULL object
#' non_null_object <- "Hello, World!"
#'
#' # This should return the original non-NULL object
#' result <- null_to_string(non_null_object)
#' result
#'
#' # Output:
#' # [1] "Hello, World!"
null_to_string <- function(x) {
  ifelse(is.null(x), "NULL", x)
}
