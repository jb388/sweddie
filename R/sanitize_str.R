#' Sanitize strings to mitigate encoding issues
#'
#' @param string string to sanitize
#' @details Removes formatting (subscripts, superscripts, slashes) and non-standard characters from string input
#' @return ASCII//TRANSLIT encoded string
#' @keywords internal
sanitize_str <- function(string) {
  string <- iconv(string, from = "", to = "ASCII//TRANSLIT")
  gsub("[^A-Za-z0-9._-]+", "_", string)
}
