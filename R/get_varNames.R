#' Helper for retrieving unique variable names
#'
#' @param meta object returned by compile_meta
#' @export
get_varNames <- function(meta = NULL) {
  if (is.null(meta)) {
    meta <- compile_meta()
  }
  unique(unlist(lapply(lapply(meta, "[[", "flmd"), function(x) lapply(x, "[[", "varName"))))
}
