#' Read template files
#'
#' @param exp_names list of directories containing SWEDDIE data
#' @return A list with core template tables or core meta data
#' @description Wrapper function to run fx \code{\link{read_meta}} on all files in list of directories specified by "exp_names"
#' @details Each directory in the list 'exp_names' must contain the directories named "input_data" and "meta", per \code{\link{read_meta}} specification
get_FLMD_dd <- function(exp_names = NULL) {
  if (missing(exp_names)) {
    exp_names <- list.files(path.expand("~/eco-warm/data/experiments"))
  }
  flmd_dd.ls <- lapply(
    lapply(exp_names, read_meta, verbose = TRUE), function(x) {
      if (length(x$flmd) == 0) {
        NULL
      } else {
        x
      }
    })
  names(flmd_dd.ls) <- exp_names
  Filter(Negate(is.null), flmd_dd.ls)
}
