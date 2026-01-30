#' Utility for stripping CSV and GZ extensions from filenames
#'
#' @param path path to file
#' @keywords internal
strip_csv_ext <- function(path) {
  sub("\\.csv(\\.gz)?$", "", path, ignore.case = TRUE)
}
