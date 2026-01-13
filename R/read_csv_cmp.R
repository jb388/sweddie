#' Helper for reading CSV and GZ files
#'
#' @param path path to file
#' @param ... optional arguments supplied to read.csv
#' @return file
#' @details runs 'read.csv' and decompresses GZ files as needed
#' @importFrom utils read.csv
#' @export
read_csv_cmp <- function(path, ...) {
  if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    con <- gzfile(path, open = "rt")
    on.exit(close(con), add = TRUE)
    read.csv(con, ...)
  } else {
    read.csv(path, ...)
  }
}
