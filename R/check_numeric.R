#' Check that a column is strictly numeric.
#'
#' @param x Column values, a vector
#' @param xname Column name
#' @return Nothing (run for its warning side effect).
#' @keywords internal
check_numeric <- function(x, xname) {
  stopifnot(is.character(xname))

  if (!is.numeric(utils::type.convert(x, as.is = FALSE))) {
    warning("Non-numeric values in ", xname, " column")
  }
}
