#' Package-level logging utility
#'
#' Write formatted messages to the console and/or a log file according to the
#' current SWEDDIE logging configuration. Logging behavior (verbosity, output
#' destination, and append mode) is controlled globally via
#' \code{\link{sweddie_set_logging}} and applied consistently across all package
#' functions.
#'
#' This function is intended for internal use only and should not be called
#' directly by users.
#'
#' @param ... Character vectors or objects coercible to character, passed to
#'   \code{\link[base]{cat}}.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @keywords internal
#' @seealso \code{\link{sweddie_set_logging}}
#'
#' @examples
#' \dontrun{
#' vcat("Starting data ingestion")
#' vcat("Processed", n, "files")
#' }
vcat <- function(...) {
  opts <- .sweddie_log_opts

  if (!isTRUE(opts$verbose))
    return(invisible(NULL))

  cat(
    ...,
    "\n",
    file   = opts$file,
    append = opts$append
  )
}
