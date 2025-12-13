#' Configure SWEDDIE logging behavior
#'
#' @param verbose logical; print messages to console?
#' @param file character; path to log file ("" = console only)
#' @param append logical; append to existing log file?
#'
#' @export
sweddie_set_logging <- function(
    verbose = NULL,
    file = NULL,
    append = NULL
) {
  if (!is.null(verbose))
    .sweddie_log_opts$verbose <- isTRUE(verbose)

  if (!is.null(file))
    .sweddie_log_opts$file <- as.character(file)

  if (!is.null(append))
    .sweddie_log_opts$append <- isTRUE(append)

  invisible(.sweddie_log_opts)
}
