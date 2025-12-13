#' Add carriage return to .csv files
#'
#' @param dataDir directory containing .csv files
#' @description Adds a carriage return (end of line character) to .csv files. Note that this is recursive by default. This behavior can be changed by setting "recursive = FALSE".
#' @return modified .csv files
#' @keywords internal
#' @importFrom utils write.table
cr_add <- function(dataDir, ..) {
  ls <- list.files(path = dataDir, pattern = ".csv$", recursive = TRUE, full.names = TRUE)
  sapply(ls, function(file) write.table(
    "", file = file, sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE))
}
