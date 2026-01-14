#' Compile all SWEDDIE data
#'
#' @param DIR local directory for SWEDDIE data files (default: "~/sweddie_db/sweddie)
#' @param pattern regex pattern for files to read (defaults to CSV and GZ)
#' @param read_fun function to call for reading files (defaults to
#' 'sweddie::read_csv_comp')
#' @param verbose should function progress be passed to console?
#' @param ... optional arguments to pass to read_fun
#' @return list
#' @export
#' @description General function for compiling all data files within the
#' specified directory into a list object. Each recursive directory is treated
#' as a subsequent list element and each file within a directory as a data frame
#' element within that list. Be wary of running this on the entire SWEDDIE
#' database as it will be very slow. Observational data are best retrieved with
#' filtering queries.
compile_all <- function(
    DIR = "~/sweddie_db/sweddie",
    pattern = "\\.csv(\\.gz)?$",
    read_fun = read_csv_cmp,
    verbose = TRUE,
    ...
) {

  stopifnot(dir.exists(DIR))

  if (verbose) {
    message("Reading directory: ", file.path(DIR))
  }

  out <- list()
  entries <- list.files(DIR, full.names = TRUE)

  for (entry in entries) {

    if (dir.exists(entry)) {

      out[[basename(entry)]] <- compile_all(
        entry,
        pattern = pattern,
        read_fun = read_fun,
        verbose = verbose,
        ...
      )

    } else if (grepl(pattern, entry, ignore.case = TRUE)) {

      nm <- sub(pattern, "", basename(entry), ignore.case = TRUE)

      if (verbose) {
        message("  └─ reading file: ", basename(entry))
      }

      out[[nm]] <- read_fun(entry, ...)
    }
  }

  out[lengths(out) > 0]
}
