#' Merge plot table for SWEDDIE database object
#'
#' @param db SWEDDIE database object
#' @param treatment Enter treatment name. Options are "warming only" (default) or "all".
#' @param plt_cols Specify columns to fill from plot table
#' @details Function for merging plot table values with data tables in a SWEDDIE database object. Optional filtering for specific treatments.
#' @importFrom stats na.omit
#' @importFrom lubridate ymd_hms
#' @export
filPltTbl.fx <- function(db, treatment = "warming only", plt_cols = NULL) {

  setNames(lapply(seq_along(db), function(i) {

    # get plot table and remove empty cols
    pltTbl <- db[[i]]$plot %>%
      select_if(~!(all(is.na(.))))

    # check plt_cols
    if (is.null(plt_cols)) {
      plt_cols <- c("plt_name")
    }

    if (any(!plt_cols %in% names(pltTbl))) {
      warning(paste0("plt_col names not found in ", names(db)[i], " plot table. Missing cols: ", plt_cols[which(!plt_cols %in% names(pltTbl))], "\n"))
    }

    # add heating level to 'plt_treat_heat'
    pltTbl$plt_treat_heat <- ifelse(pltTbl$plt_heat_level == 0, "control", paste0("treatment", pltTbl$plt_heat_level))

    # check for additional treatments
    if (any(grepl("plt_treat_add_name", names(pltTbl)))) {

      # fill blank 'plt_treat_add_name' w/ NA
      pltTbl$plt_treat_add_name <- ifelse(pltTbl$plt_treat_add_name == "", NA, pltTbl$plt_treat_add_name)

      # Split any semicolon-separated values
      split_values <- strsplit(
        as.character(pltTbl$plt_treat_add_name),
        ";",
        fixed = TRUE)
      split_values_lvl <- strsplit(
        as.character(pltTbl$plt_treat_add_level),
        ";",
        fixed = TRUE)

      # Determine the maximum number of values
      max_splits <- max(sapply(split_values, length))

      # get indices of base treatment/control pairs (heat_only)
      ix.base <- vector(mode = "list", length = max_splits)

      # Create additional columns for each split value
      for (j in seq_len(max_splits)) {
        pltTbl[[paste0("plt_treat_add_name_", j)]] <- sapply(split_values, function(v) ifelse(length(v) >= j, v[j], NA))
        pltTbl[[paste0("plt_treat_add_level_", j)]] <- sapply(split_values_lvl, function(v) ifelse(length(v) >= j, v[j], NA))
        pltTbl[[paste0("plt_treat_add_name_", j)]] <- ifelse(
          is.na(pltTbl[[paste0("plt_treat_add_name_", j)]]), NA,
          paste0(pltTbl[[paste0("plt_treat_add_name_", j)]], "_", pltTbl[[paste0("plt_treat_add_level_", j)]]))
        ix.base[[j]] <- which(pltTbl[[paste0("plt_treat_add_level_", j)]] == 0 | is.na(pltTbl[[paste0("plt_treat_add_level_", j)]]))
        pltTbl$plt_treat_add_name <- NULL
      }

      interaction_cols <- c("plt_treat_heat", names(pltTbl)[grep("plt_treat_add_name", names(pltTbl))])
      pltTbl$plt_treat <- apply(
        pltTbl[, interaction_cols, drop = TRUE], 1, function(row) paste(na.omit(row), collapse = "_"))
    } else {
      names(pltTbl)[which(names(pltTbl) == "plt_treat_heat")] <- "plt_treat"
    }

    if (treatment == "warming") {
      pltTbl <- pltTbl[pltTbl$plt_treat_add_level == 0, ]
    }

    # merge with data
    lapply(seq_along(db[[i]]$data), function(j) {

      x <- db[[i]]$data[[j]]

      # get plot table indices and fill plot table cols
      ix <- match(x$plt_name, pltTbl$plt_name)

      # filling
      plt_cols <- c(plt_cols, "plt_treat")
      for (k in seq_along(plt_cols)) {
        x[[plt_cols[k]]] <- pltTbl[[plt_cols[k]]][ix]
      }

      # parse date/time
      if (!any(is.POSIXct(x$date) | is.Date(x$date))) {
        x$date <- ymd_hms(x$date, truncated = 5)
      }
      x
    })
  }), nm = names(db))
}
