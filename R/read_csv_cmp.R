#' Helper for reading CSV and GZ files
#'
#' @param path path to file
#' @param ... optional arguments supplied to read.csv
#' @return file
#' @details runs 'read.csv' and decompresses GZ files as needed
#' @importFrom utils read.csv
#' @export
read_csv_cmp <- function(path, ...) {

  # If path has no extension, try .csv then .csv.gz
  if (!grepl("\\.(csv|csv\\.gz)$", path, ignore.case = TRUE)) {
    path_csv <- paste0(path, ".csv")
    path_csv_gz <- paste0(path, ".csv.gz")

    if (file.exists(path_csv)) {
      path <- path_csv
    } else if (file.exists(path_csv_gz)) {
      path <- path_csv_gz
    } else {
      stop("File not found: ", path, "(.csv or .csv.gz)")
    }
  }

  if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    con <- gzfile(path, open = "rt")
    on.exit(close(con), add = TRUE)
    read.csv(con, ...)
  } else {
    read.csv(path, ...)
  }
}
