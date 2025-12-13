#' Check whether an object contains SWEDDIE core tables
#'
#' @param x The object to check
#' @description A valid SWEDDIE object is a list with the following elements,
#' all of which must be \code{\link{data.frame}} objects:
#' \itemize{
#' \item{\code{experiment}}{ Experiment}
#' \item{\code{site}}{ Site}
#' \item{\code{plot}}{ Plot}
#' }
#' @return TRUE or FALSE.
#' @keywords internal
check_sweddie_core <- function(x) {
  # Database is a list and must have all the following data frames
  tables <- c(
    "experiment", "site", "plot"
  )
  is.list(x) &&
    identical(sort(tables), sort(names(x))) &&
    all(sapply(x, class) == "data.frame")
}
