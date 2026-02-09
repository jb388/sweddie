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

  # guess encoding
  enc_guess <- tryCatch(
    readr::guess_encoding(path, n_max = 10000)$encoding[1],
    error = function(e) "UTF-8"
  )

  # get ...
  base_args <- list(...)

  read_attempt <- function(enc) {
    args <- c(base_args, list(fileEncoding = enc))

    if (grepl("\\.gz$", path, ignore.case = TRUE)) {
      con <- gzfile(path, open = "rt")
      on.exit(close(con), add = TRUE)
      args$file <- con
    } else {
      args$file <- path
    }

    do.call(read.csv, args)
  }

  tryCatch(
    read_attempt(enc_guess),
    error = function(e) {
      message("Retrying with Windows-1252 encoding")
      read_attempt("Windows-1252")
    }
  )
}
