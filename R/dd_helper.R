#' Helper for creating data dictionary files
#'
#' @param expName name of experiment
#' @param dataName input data to check
#' @param DATA_DIR directory to search for .csv file matching supplied dataName
#' @details this function creates a data dictionary template file for the supplied input data
#' @export
dd_helper <- function(expName, dataName, DATA_DIR = NULL, write_out = TRUE) {
  if (is.null(DATA_DIR)) {
    DATA_DIR <- file.path("~/eco-warm/data/experiments", expName, "input_data")
    META_DIR <- file.path("~/eco-warm/data/experiments", expName, "meta")
  }
  template <- read.csv(
    "~/eco-warm/data/sweddie/metadata/datTemplate_dd.csv",
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
