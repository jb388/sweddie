#' Read core template files
#'
#' @param templateDir core template directory
#' @param return should an empty template list be returned ("template") or the metadata file itself?
#' @return A list with core template tables or core meta data
#' @description loads
#' @importFrom stats setNames
#' @importFrom utils read.csv
read_core_templates <- function(templateDir, return = "template") {
  # get dir name as needed
  if (missing(templateDir)) {
    templateDir <- system.file("extdata", "templates", "core", package = "sweddie")
  }
  # create template list
  ls <- list.files(templateDir, full.names = TRUE)
  ls <- ls[grep("_dd", ls)]
  meta <- setNames(lapply(ls, read.csv), nm = c("experiment", "plot", "site"))
  if (return == "template") {
    lapply(
      meta, function(x) {
        setNames(data.frame(matrix(ncol = nrow(x), nrow = 0)), x[ , 1])
      })
  } else {
    return(meta)
  }
}
