#' Compile SWEDDIE data
#'
#' @param DIR Local directory for SWEDDIE files
#' @param varNames Optional vector of variable names. Names supplied must be an exact match with the values returned by 'get_varNames'. If NULL (default), user will be prompted to select variables via console prompt.
#' @param exp_name Name of experiment for which to compile data. Default (NULL) returns data from all experiments.
#' @param verbose Should function progress be passed to console?
#' @return list
#' @export
#' @description General function for compiling SWEDDIE data. The resulting list object will reflect the directory structure, i.e., each recursive directory is treated
#' as a subsequent list element and each (CSV or compressed CSV) file within a directory as a data frame
#' element within that list. Be wary of compiling all available variables as this will be very slow.
compile_sweddie <- function(DIR = "~/sweddie_db", varNames = NULL, exp_name = NULL, verbose = TRUE) {

  DB_DIR <- file.path(DIR, "sweddie")
  stopifnot(dir.exists(DB_DIR))

  # get metadata
  meta <- compile_meta(DIR, EOL_err = TRUE)

  if (!is.null(exp_name)) {
    meta <- meta[exp_name]
  }

  if (!length(meta)) stop("No metadata returned by compile_meta()")

  # define varName query
  if (is.null(varNames)) {
    varNames <- get_varNames(meta)
    while (TRUE) {

      # user defined varNames
      print(varNames)
      input <- readline(prompt=paste0("Which variables would you like to compile? Enter the indices or index from the above list. "))

      # Check if the user wants to cancel
      if (input == '0') {
        return(NULL)
      }

      # Try to convert input to numeric
      ix.in <- unlist(strsplit(input, ","))
      ix.cln <- sapply(ix.in, grepl, pattern = ":")
      ix.csv <- as.numeric(ix.in[which(!ix.cln)])

      if(any(ix.cln)) {
        ix.rng <- unlist(lapply(sapply(
          ix.in[which(ix.cln)], strsplit, ":"), function(x) {
            seq(x[1], x[2])
          }), use.names = FALSE)
        ix <- c(ix.csv, ix.rng)
      } else {
        ix <- ix.csv
      }

      # Check if the indices are valid
      if (all(!is.na(ix)) && all(ix >= 1) && all(ix <= length(varNames))) {
        return(ix)
      } else {
        cat("Error: Invalid indices. Please ensure the indices are numeric and within the range (1 to ", length(varNames), ").\n")
      }
    }
    selVars <- varNames[ix]
  } else {
    selVars <- varNames
  }

  # Check if any requested varName missing across all experiments
  missing <- setdiff(selVars, unlist(lapply(meta, function(x) x$flmd$flmd$varName)))
  if (length(missing)) {
    stop("Requested varName(s) not found in any FLMD: ", paste(missing, collapse = ", "))
  }

  # Map each experiment to the FLMD table
  flmd_ls <- setNames(lapply(names(meta), function(exp) {
    flmd_tbl <- meta[[exp]]$flmd$flmd
    if (!"varName" %in% names(flmd_tbl) || !"fileName" %in% names(flmd_tbl)) {
      if (verbose) message("FLMD table missing 'varName' or 'fileName' columns for experiment ", exp)
    }
    # Match requested varName to varName in flmd
    flmd_tbl[flmd_tbl$varName %in% selVars, ]
  }), nm = names(meta))
  flmd_ls <- Filter(function(x) !is.null(x) && nrow(x) > 0, flmd_ls)

  # select data files
  flmd_ls_filtered <- vector("list", length(flmd_ls))
  names(flmd_ls_filtered) <- names(flmd_ls)
  for (exp_name in names(flmd_ls_filtered)) {

    flmd_df <- flmd_ls[[exp_name]]
    by_var <- split(flmd_df, flmd_df$varName)
    keep_rows <- lapply(names(by_var), function(vn) {

      subdf <- by_var[[vn]]
      if (nrow(subdf) == 1) return(subdf)
      cat("\nVariable:", vn,
          "\nExperiment:", exp_name,
          "\nMultiple files detected:\n")

      print(data.frame(
        index = seq_len(nrow(subdf)),
        fileName = subdf$fileName,
        sit = subdf$sit_name,
        start = subdf$startDate,
        end = subdf$endDate
      ), row.names = FALSE)

      repeat {
        input <- readline(
          prompt = "Select file indices to ingest (comma or range, 0=cancel): "
        )

        if (input == "0") stop("User cancelled.")
        ix.in <- unlist(strsplit(input, ","))
        ix.cln <- grepl(":", ix.in)
        ix.csv <- as.numeric(ix.in[!ix.cln])

        if (any(ix.cln)) {
          ix.rng <- unlist(lapply(strsplit(ix.in[ix.cln], ":"), function(x)
            seq(as.numeric(x[1]), as.numeric(x[2]))))
          ix <- c(ix.csv, ix.rng)
        } else {
          ix <- ix.csv
        }

        if (all(!is.na(ix)) && all(ix >= 1 & ix <= nrow(subdf))) {
          return(subdf[ix, , drop = FALSE])
        }

        cat("Invalid selection. Try again.\n")
      }

    })

    flmd_ls_filtered[[exp_name]] <- do.call(rbind, keep_rows)
  }

  if (verbose) {
    message("Compiling requested variables: ", paste(selVars, collapse = ", "))
  }

  setNames(lapply(seq_along(flmd_ls_filtered), function(i) {

    exp_name <- names(flmd_ls_filtered)[i]
    exp_path <- file.path(DB_DIR, exp_name)
    flmd_df <- flmd_ls_filtered[[i]]
    n_files  <- nrow(flmd_df)

    if (verbose) {
      message(
        "\n===== Compiling experiment: ", exp_name,
        " (", n_files, " file", if (n_files != 1) "s", ") ====="
      )
    }

    data <- setNames(vector("list", n_files), flmd_df$fileName)
    dd <- setNames(vector("list", n_files), flmd_df$fileName)

    for (j in seq_len(n_files)) {
      fileName <- flmd_df$fileName[j]
      ddName <- sub("\\.csv.*$", "_dd.csv", fileName)

      if (verbose) {
        message("  [", j, "/", n_files, "] ", fileName)
      }

      data[[j]] <- read_csv_cmp(file.path(exp_path, "data", fileName))
      dd[[j]] <- read.csv(file.path(exp_path, "dd", ddName))
    }

    list(
      experiment = read.csv(file.path(exp_path, "experiment.csv")),
      site = read.csv(file.path(exp_path, "site.csv")),
      plot = read.csv(file.path(exp_path, "plot.csv")),
      data = data,
      dd = dd,
      flmd = list(flmd = flmd_df))

  }), nm = names(flmd_ls_filtered))
}
