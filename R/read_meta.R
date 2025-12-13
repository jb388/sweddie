#' Read template files
#'
#' @param metaDir core template directory
#' @param return should an empty template list be returned ("template") or the meta data file itself?
#' @return A list with core template tables or core meta data
read_meta <- function(metaDir, return = "template") {
  # get dir name as needed
  if (missing(metaDir)) {
    metaDir <- "~/eco-warm/data/sweddie/metadata/core"
  }
  # create template list
  ls <- list.files(metaDir, full.names = TRUE)
  ls <- ls[grep("_dd", ls)]
  meta <- lapply(
    setNames(ls, nm = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(ls))),
    read.csv)
  if (return == "template") {
    lapply(
      setNames(meta, nm = c("experiment", "plot", "site")), function(x) {
        setNames(data.frame(matrix(ncol = nrow(x), nrow = 0)), x[ , 1])
      })
  } else {
    return(meta)
  }
}
