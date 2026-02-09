#' Helper for reading CSV and GZ files
#'
#' @param path path to file
#' @param ... optional arguments supplied to read.csv
#' @return file
#' @details runs 'read.csv' and decompresses GZ files as needed
#' @importFrom utils read.csv
#' @importFrom readr guess_encoding
#' @export
read_csv_cmp <- function(path, ...) {

  # resolve extension
  if (!grepl("\\.(csv|csv\\.gz)$", path, ignore.case = TRUE)) {
    if (file.exists(paste0(path, ".csv"))) {
      path <- paste0(path, ".csv")
    } else if (file.exists(paste0(path, ".csv.gz"))) {
      path <- paste0(path, ".csv.gz")
    } else {
      stop("File not found: ", path)
    }
  }

  # read file
  if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    # for gz files: read as raw Latin1 to avoid hangs
    con <- gzfile(path, "rt")
    on.exit(close(con), add = TRUE)
    out <- tryCatch(
      read.csv(con, fileEncoding = "latin1", ..., stringsAsFactors = FALSE, quote = ""),
      error = function(e) stop("Failed to read gz file: ", path, "\n", e$message)
    )
  } else {
    # regular files: try UTF-8 first, fallback to Latin1
    out <- tryCatch(
      read.csv(path, fileEncoding = "UTF-8", ..., stringsAsFactors = FALSE, quote = ""),
      error = function(e) {
        read.csv(path, fileEncoding = "latin1", ..., stringsAsFactors = FALSE, quote = "")
      }
    )
  }

  # convert to UTF-8 to normalize
  out[] <- lapply(out, function(col) {
    if (is.character(col)) iconv(col, from = "latin1", to = "UTF-8")
    else col
  })

  out
}
