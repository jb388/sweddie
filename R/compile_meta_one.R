#' Compile SWEDDIE metadata for single experiment
#'
#' @param DIR local directory for SWEDDIE database
#' @param expName name of experiment to retrieve metadata from; must match standard names
#' @param write_report logical; should report be written to a file?
#' @param verbose logical
#' @return list
#' @export
#' @description returns SWEDDIE metadata object
#' @importFrom utils glob2rx
compile_meta_one <- function(DIR, expName) {

vcat(
  "\n",
  paste0("===== Compiling metadata for experiment: ", expName, " =====\n"),
  rep("-", 60),
  "\n"
)



}
