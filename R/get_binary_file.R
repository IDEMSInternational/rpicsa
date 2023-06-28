#' Get Binary File
#'
#' Reads and returns the binary data of a file.
#'
#' @param file_path The path to the file.
#' @return A raw vector containing the binary data of the file.
#' 
#' @examples # to complete
#' #pdf_data <- get_binary_file("data/idems_logo.jpg")
#' #pdf_data <- get_binary_file("data/idems_logo.pdf")
#'
#' @export
get_binary_file <- function(file_path) {
  binary_data <- readBin(file_path, "raw", file.info(file_path)$size)
  return(binary_data)
}