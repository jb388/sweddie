#' Check uniqueness of observations based on key columns
#'
#' @param dat data.frame containing observations
#' @param ix.key integer vector of column indices forming the uniqueness key
#' @param verbose logical; print diagnostic messages
#' @param return_dups logical; return indices of duplicate rows
#'
#' @return logical vector indicating duplicate rows, or integer indices if return_dups = TRUE
#' @export
check_unique_records <- function(dat,
                                 ix.key,
                                 verbose = TRUE,
                                 return_dups = FALSE) {

  # drop NULLs safely
  ix.key <- ix.key[!vapply(ix.key, is.null, logical(1))]

  if (length(ix.key) == 0) {
    stop("No key columns supplied for uniqueness check.")
  }

  key_names <- names(dat)[ix.key]

  if (verbose) {
    message(
      "Checking uniqueness of observations using: ",
      paste(key_names, collapse = " × ")
    )
  }

  # collapse key columns into a single hash per row
  key <- do.call(
    paste,
    c(dat[ix.key], sep = "\r")  # rare separator to avoid collisions
  )

  dup <- duplicated(key)

  if (any(dup) && verbose) {
    message(
      sum(dup),
      " duplicate record(s) detected."
    )
  }

  if (return_dups) {
    return(which(dup))
  }

  dup
}
