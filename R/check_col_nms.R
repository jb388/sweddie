#' Check that column names in the template and meta data files match.
#'
#' @param metadata list of core template files
#' @param tableName specific table name to check
#' @param datIn input data to check
#' @param err error counter; defaults to 0
#' @return error counter "err"
#' @note This is typically called only from \code{\link{checkColNms}}.
#' @keywords internal
# checks column names
checkColNms <- function(tableName, datIn, err, file = outfile) {

  vcat("\t", tableName, "\n")

  # check for template
  metadata <- readCore(return = "dd")

  # check that required data files are present (experiment, site, plot)
  if (!any(grepl(tableName, names(datIn)))) {
    err <- err + 1
    vcat("\t", tableName, "is missing in metadata/siteData directory\n")
  }

  # check that required columns are present
  req <- metadata[[tableName]][which(metadata[[tableName]][["req"]] == "yes"), 1]
  for (i in seq_along(req)) {
    if (!any(grepl(req[i], names(datIn[[tableName]])))) {
      err <- err + 1
      vcat("\t\t", "Required column", req[i], "is missing in", tableName, "table\n")
    }
  }

  # check for missing cols in data
  miss <- setdiff(metadata[[tableName]][["col_name"]], colnames(datIn[[tableName]]))
  miss <- miss[!(miss %in% req)]

  # check for extra cols in data
  xtra <- setdiff(colnames(datIn[[tableName]]), metadata[[tableName]][["col_name"]])

  if (length(miss) > 0 ) {
    vcat("\t\t Non-required columns missing from data:", miss, "\n")
  }
  if (length(xtra > 0)) {
    xtras <- sapply(seq_along(xtra), function(i) {
      ix <- which(colnames(datIn[[tableName]]) == xtra[i])
      paste0(xtra[i], " (", ix, ")")
    })
    vcat("\t\t Extra columns:", xtras, "\n")
  }
  return(err)
}
