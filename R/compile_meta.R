#' Compile SWEDDIE metadata
#'
#' @param DIR local directory for SWEDDIE
#' @param expName name(s) of experiment(s) to retrieve metadata from; must match standard names
#' @param write_report logical; should report be written to a file?
#' @param verbose logical
#' @param EOL_err set to true if you encounter an end of line (EOL) error
#' @return list
#' @export
#' @description returns SWEDDIE metadata object
#' @importFrom utils glob2rx
compile_meta <- function(DIR = "~/sweddie_db",
                         expName = NULL,
                         verbose = FALSE,
                         write_report = FALSE,
                         EOL_err = FALSE) {

  # --- Logging setup ---
  if (write_report) {
    TIMESTAMP <- format(Sys.time(), "%y%m%d-%H%M")
    outfile <- file.path(DIR, "build_logs", paste0("metaLog_", TIMESTAMP, ".txt"))
    invisible(file.create(outfile))
    .sweddie_log_opts$append <- TRUE
    .sweddie_log_opts$file   <- outfile

    # --- Main log header ---
    vcat(
      "SWEDDIE Metadata Log\n\n",
      paste0(" ", Sys.time(), "\n"),
      paste(rep("-", 30), collapse = ""), "\n\n"
    )

  } else {
    .sweddie_log_opts$append <- FALSE
    .sweddie_log_opts$file   <- ""
  }
  .sweddie_log_opts$verbose <- verbose

  # --- Internal worker for a single experiment ---
  .compile_meta_one <- function(DIR, expName) {

    # Header
    vcat(
      "\n",
      paste0("===== Compiling metadata for experiment: ", expName, " =====\n"),
      rep("-", 60),
      "\n"
    )

    # get site dir paths
    exp.ls <- list.dirs(file.path(DIR, "sweddie"), recursive = FALSE)
    exp.dir <- exp.ls[match(expName, basename(exp.ls))]

    if (!dir.exists(exp.dir)) {
      vcat("Directory does not exist:", exp.dir, "\n")
      return(NULL)
    }

    vcat("\n\nCompiling metadata files in", exp.dir, "\n", rep("-", 30), "\n")

    # Get file paths
    exp.dir.ls <- list.files(exp.dir, recursive = TRUE, full.names = TRUE)

    # FLMD and DD files
    flmd.ls <- exp.dir.ls[grepl("flmd", exp.dir.ls)]
    dat.ls <- list.files(file.path(exp.dir, "data"), full.names = TRUE)
    dd.ls   <- list.files(file.path(exp.dir, "dd"), full.names = TRUE)

    # ensure EOL carriage return present
    if (EOL_err) invisible(cr_add(exp.dir))

    if (length(dd.ls) == 0) {
      vcat("No DD files found for experiment:", expName, "\n")
      return(NULL)
    }

    if (length(flmd.ls) == 0) {
      vcat("No FLMD files found for experiment:", expName, "\n")
      return(NULL)
    }

    if (length(flmd.ls) > 1) {
      vcat("\nExperiment directory contains multiple FLMD files but only one is allowed:\n",
           basename(flmd.ls), "\n")
      return(NULL)
    }

    # Match data and dd files
    dat_names <- basename(dat.ls)
    dd_names <- basename(gsub("_dd", "", dd.ls))

    missing_dd <- setdiff(dat_names, dd_names)
    dat.clean.ls <- dat.ls[!(dat_names %in% missing_dd)]
    extra_dd <- setdiff(dd_names, dat_names)
    dd.clean.ls <- dd.ls[!(dd_names %in% extra_dd)]

    if (length(missing_dd) > 0) {
      vcat("\tThe following input data files are missing data dictionaries and will not be ingested:\n",
           paste(missing_dd, collapse = ", "), "\n")
    }

    if (length(extra_dd) > 0) {
      vcat("\tThe following data dictionary files are missing input data and will not be ingested:\n",
           paste(extra_dd, collapse = ", "), "\n")
    }

    # If no files remain after filtering
    if (length(dd.clean.ls) == 0) {
      vcat("No valid DD files remain after filtering for experiment:", expName, "\n")
      return(NULL)
    }

    # Read files
    dd <- lapply(dd.ls, read.csv, strip.white = TRUE)
    names(dd) <- gsub("\\.csv", "", basename(dd.ls))
    flmd <- lapply(flmd.ls, read.csv, strip.white = TRUE)
    names(flmd) <- gsub("\\.csv", "", basename(flmd.ls))

    # Check input files against FLMD
    if (length(dat.ls) > 0) {
      ix <- which(unlist(lapply(lapply(basename(dat.ls), function(x)
        unlist(lapply(flmd, function(y) lapply(y$fileName, function(z) grep(glob2rx(z), x))))),
        function(d) length(d) == 0)))
      if (length(ix) > 0) {
        vcat("\tThe following input data files are missing FLMD and will not be ingested:\n",
             basename(dat.ls)[ix], "\n")
        dat.ls <- dat.ls[-ix]
      }
    }

    # Check columns
    dat.ls.colNms <- lapply(setNames(dat.ls, basename(dat.ls)), get_CSV_nms)
    for (i in seq_along(dat.ls.colNms)) {
      miss <- dat.ls.colNms[[i]][!(dat.ls.colNms[[i]] %in% dd[[i]][["colName"]])]
      if (length(miss) > 0) {
        vcat("Data dictionary file '", basename(dd.ls[i]), "' missing column/s: ", miss, "\n")
      }
    }

    return(list(flmd = flmd, dd = dd))
  }

  if (is.null(expName)) {
    expName <- basename(list.dirs(file.path(DIR, "sweddie"), recursive = FALSE))
  }

  # --- Batch/vectorized execution ---
  if (length(expName) > 1) {
    res <- lapply(expName, function(x) {
      tryCatch(
        .compile_meta_one(DIR, x),
        error = function(e) {
          vcat("ERROR processing experiment:", x, "-", e$message, "\n")
          NULL
        }
      )
    })
    names(res) <- expName
    return(res)
  }

  # Single experiment
  .compile_meta_one(DIR, expName)
}
