#' Add Parameters to a Function Expression
#'
#' This function takes a function expression and a list of additional parameters 
#' and appends these parameters to the function expression. It is useful for 
#' dynamically modifying function expressions by adding extra parameters as needed.
#'
#' @param fn_exp A character string representing the function expression to which 
#'               you want to add parameters.
#' @param add_params A list of additional parameters to be added to the function 
#'                   expression. Each parameter should be specified as a named 
#'                   element in the list, where the name is the parameter name, 
#'                   and the value is the parameter's value.
#'
#' @return A character string representing the modified function expression with 
#'         the additional parameters added.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' 
#' # Define a function expression without parameters
#' fn_expression <- "my_function()"
#'
#' # Add a single parameter to the expression
#' modified_expression <- add_params(fn_expression, list(param1 = 42))
#' modified_expression
#'
#' # Output:
#' # [1] "my_function(), param1 = 42"
#'
#' # Add multiple parameters to the expression
#' modified_expression <- add_params(fn_expression, list(param1 = 42, param2 = "abc"))
#' modified_expression
#'
#' # Output:
#' # [1] "my_function(), param1 = 42, param2 = abc"
add_params <- function(fn_exp, add_params = list()) {
  for (i in seq_along(add_params)) {
    fn_exp <- paste0(fn_exp, ", ", names(add_params)[i], " = ", add_params[[i]])
  }
  fn_exp
}
