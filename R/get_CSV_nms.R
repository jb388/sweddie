#' Get csv names
#'
#' @param path.csv path to csv file
#' @return names of csv file columns
#' @keywords internal
#' @description allows name checking without loading whole file
get_CSV_nms <- function (path.csv) {
  names(read.csv(path.csv, nrows = 1, check.names = FALSE, strip.white = TRUE))
}
