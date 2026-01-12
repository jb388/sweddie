#' Helper for creating data dictionary files
#'
#' @param expName name of experiment
#' @param dataName input data to check
#' @param DATA_DIR directory to search for .csv file matching supplied dataName
#' @param write_out should function write the template file to disk?
#' @return if write_out = FALSE, data frame of dd template
#' @details this function creates a data dictionary template file for the supplied input data
#' @importFrom utils read.csv write.csv
#' @export
dd_helper <- function(expName, dataName, DATA_DIR = NULL, META_DIR = NULL, write_out = TRUE) {
  if (is.null(DATA_DIR)) {
    DATA_DIR <- file.path("~/sweddie_db/sweddie", expName, "dat", "data")
  }
  if (is.null(META_DIR)) {
    META_DIR <- file.path("~/sweddie_db/sweddie", expName, "dat", "meta")
  }
  template <- read.csv(
    system.file("extdata", "templates", "meta", "datTemplate_dd.csv", package = "sweddie"),
    stringsAsFactors = FALSE,
    check.names = FALSE)
  data <- read.csv(
    file.path(DATA_DIR, paste0(dataName, ".csv")),
    nrows = 1,
    check.names = FALSE) # this prevents replacement of characters to be R friendly
  template[1:ncol(data), 1] <- names(data)
  if (write_out) {
    write.csv(x = template,
              file = file.path(META_DIR, paste0(dataName, "_dd.csv")),
              row.names = FALSE)
  } else {
    template
  }
}
