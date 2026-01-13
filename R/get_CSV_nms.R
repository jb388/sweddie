#' Get csv names
#'
#' @param path path to CSV or GZ file
#' @return names of csv file columns
#' @keywords internal
#' @description allows name checking without loading whole file
get_CSV_nms <- function (path) {
  names(read_csv_cmp(path, nrows = 1, check.names = FALSE, strip.white = TRUE))
}
