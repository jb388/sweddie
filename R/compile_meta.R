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

  # helper fx to remove extensions
  strip_csv_ext <- function(x) {
    sub("\\.csv(\\.gz)?$", "", x, ignore.case = TRUE)
  }

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
  compile_one <- function(DIR, expName) {

    # Header
    vcat(
      "\n",
      paste0("===== Compiling metadata for experiment: ", expName, " =====\n"),
      rep("-", 60),
      "\n"
    )

    # get site dir paths
    exp.dir <- file.path(DIR, "sweddie", expName)

    if (!dir.exists(exp.dir)) {
      vcat("Directory does not exist:", exp.dir, "\n")
      return(NULL)
    }

    vcat("\n\nCompiling metadata files in", exp.dir, "\n", rep("-", 30), "\n")

    # Get file paths
    dat.ls  <- list.files(file.path(exp.dir, "data"), full.names = TRUE)
    dd.ls   <- list.files(file.path(exp.dir, "dd"),   full.names = TRUE)
    flmd.ls <- list.files(exp.dir, recursive = TRUE, full.names = TRUE)
    flmd.ls <- flmd.ls[grepl("flmd", flmd.ls)]

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
    dat_base <- strip_csv_ext(basename(dat.ls))
    dd_base <- gsub("_dd$", "", strip_csv_ext(basename(dd.ls)))
    missing_dd <- setdiff(dat_base, dd_base)
    extra_dd   <- setdiff(dd_base, dat_base)

    if (length(missing_dd)) {
      vcat("\tThe following input data files are missing data dictionaries and will not be ingested:\n",
           paste(missing_dd, collapse = ", "), "\n")
    }

    if (length(extra_dd)) {
      vcat("\tThe following data dictionary files are missing input data and will not be ingested:\n",
           paste(extra_dd, collapse = ", "), "\n")
    }

    # If no files remain after filtering
    keep <- intersect(dat_base, dd_base)
    if (!length(keep)) {
      vcat("No valid DD files remain after filtering for experiment:", expName, "\n")
      return(NULL)
    }

    # filtered lists
    dat.ls <- dat.ls[dat_base %in% keep]
    dd.ls  <- dd.ls[dd_base  %in% keep]

    # Read files
    dd <- setNames(
      lapply(dd.ls, read_csv_cmp),
      gsub("_dd$", "", strip_csv_ext(basename(dd.ls)))
    )
    flmd <- setNames(
      lapply(flmd.ls, read_csv_cmp),
      strip_csv_ext(basename(flmd.ls))
    )

    # Check input files against FLMD
    dat_cols <- lapply(dat.ls, get_CSV_nms)
    names(dat_cols) <- strip_csv_ext(basename(dat.ls))

    valid_data <- vapply(names(dat_cols), function(dn) {
      any(strip_csv_ext(flmd[[1]]$fileName) == dn)
    }, logical(1))

    if (any(!valid_data)) {
        vcat("\tThe following input data files are missing FLMD and will not be ingested:\n",
             paste(names(valid_data)[!valid_data], collapse = ", "), "\n")
    }

    dat_cols <- dat_cols[valid_data]

    if (!length(dat_cols)) {
      vcat("No data files remain after filtering\n")
      return(NULL)
    }

    # Check columns
    for (nm in names(dat_cols)) {
      miss <- setdiff(dat_cols[[nm]], dd[[nm]]$colName)
      if (length(miss)) {
        vcat("Data dictionary file ", nm, " missing column/s: ",
             paste(miss, collapse = ", "), "\n")
      }
    }
    return(list(flmd = flmd, dd = dd))

  }

  if (is.null(expName)) {
    expName <- basename(list.dirs(file.path(DIR, "sweddie"), recursive = FALSE))
  }

  # --- Batch/vectorized execution ---
  res <- lapply(expName, function(x) {
    tryCatch(
      compile_one(DIR, x),
      error = function(e) {
        vcat("ERROR processing", x, ":", e$message, "\n")
        NULL
      }
    )
  })

  names(res) <- expName

  if (length(res) == 1) return(res[[1]])
  res
}
