#' Package load hook
#'
#' Initialize package-level options and internal state when the SWEDDIE package
#' is loaded. The function sets default logging behavior and prepares internal
#' environments required by other package functions.
#'
#' This function is called automatically by R when the package namespace is
#' loaded and should not be invoked directly.
#'
#' @param libname character; path to the library directory.
#' @param pkgname character; name of the package.
#'
#' @return NULL (called for side effects only).
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  .sweddie_log_opts <- new.env(parent = emptyenv())

  .sweddie_log_opts$verbose <- TRUE
  .sweddie_log_opts$append  <- TRUE
  .sweddie_log_opts$file    <- ""

  ## assign into package namespace
  assign(".sweddie_log_opts", .sweddie_log_opts,
         envir = parent.env(environment()))
}
