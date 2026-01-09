#' Compile SWEDDIE metadata (vectorized)
#'
#' @param DIR local directory for SWEDDIE database
#' @param expName name(s) of experiment(s) to retrieve metadata from; must match standard names
#' @param write_report logical; should report be written to a file?
#' @param verbose logical
#' @return list
#' @export
#' @description returns SWEDDIE metadata object. Supports multiple experiments.
#' @importFrom utils glob2rx
compile_meta <- function(DIR = "~/eco-warm/data",
                         expName,
                         verbose = TRUE,
                         write_report = FALSE) {

  # ---- Configure logging ----
  if (write_report) {
    TIMESTAMP <- format(Sys.time(), "%y%m%d-%H%M")
    log_dir <- file.path(DIR, "sweddie/database/logs")
    if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
    outfile <- file.path(log_dir, paste0("metaLog_", TIMESTAMP, ".txt"))
    invisible(file.create(outfile))
    .sweddie_log_opts$append <- TRUE
    .sweddie_log_opts$file <- outfile
  } else {
    .sweddie_log_opts$append <- FALSE
    .sweddie_log_opts$file <- ""
  }
  .sweddie_log_opts$verbose <- verbose

  # ---- Vectorized wrapper ----
  if (length(expName) > 1) {
    res <- lapply(expName, function(x) {
      compile_meta_one(DIR = DIR, expName = x, verbose = verbose)
    })
    names(res) <- expName
    return(res)
  }

  # ---- Single experiment ----
  compile_meta_one(DIR = DIR, expName = expName, verbose = verbose)
}

#' @keywords internal
#' Internal function: compiles metadata for a single experiment
compile_meta_one <- function(DIR, expName, verbose = TRUE) {

  # ---- Experiment-specific log header ----
  vcat(
    "\n",
    paste0("===== Compiling metadata for experiment: ", expName, " =====\n"),
    rep("-", 60),
    "\n"
  )

  # ---- Locate experiment directory ----
  exp.ls <- list.dirs(file.path(DIR, "experiments"), recursive = FALSE)
  exp.dir <- exp.ls[match(expName, basename(exp.ls))]
  stopifnot(dir.exists(exp.dir))

  if (length(list.files(exp.dir)) == 0) {
    vcat("\n", expName, "\n\n", "No files found in ", exp.dir, "\n")
    return(NULL)
  }

  # ---- Input and meta directories ----
  dat.dir <- file.path(exp.dir, "input_data")
  stopifnot(dir.exists(dat.dir))
  dat.dir.ls <- list.files(dat.dir, full.names = TRUE)

  mta.dir <- file.path(exp.dir, "meta")
  stopifnot(dir.exists(mta.dir))
  mta.dir.ls <- list.files(mta.dir, full.names = TRUE)

  # Ensure EOL carriage return present
  invisible(cr_add(mta.dir))

  # ---- Metadata files ----
  flmd.ls <- mta.dir.ls[grepl("_flmd.csv", mta.dir.ls)]
  dd.ls <- mta.dir.ls[grepl("_dd.csv", mta.dir.ls)]
  if (length(flmd.ls) == 0 | length(dd.ls) == 0) return(NULL)

  # Non-standard files
  if (sum(length(flmd.ls), length(dd.ls)) != length(mta.dir.ls)) {
    ix <- which(!(basename(mta.dir.ls) %in% c(basename(flmd.ls), basename(dd.ls))))
    vcat("\tMeta directory contains non-standard files that will be ignored:\n",
         paste(basename(mta.dir.ls)[ix], collapse = ", "), "\n")
  }

  # ---- Match input data and dd files ----
  dat.ls <- dat.dir.ls[basename(dat.dir.ls) %in% basename(gsub("_dd", "", dd.ls))]

  # Exclude dd files without matching input data
  ix <- which(is.na(match(basename(gsub("_dd", "", dd.ls)), basename(dat.dir.ls))))
  if (length(ix) > 0) {
    vcat("\tDD files missing input data and skipped:\n",
         paste(basename(dd.ls)[ix], collapse = ", "), "\n")
    dd.ls <- dd.ls[-ix]
  }

  # Exclude input files without matching dd
  ix <- which(is.na(match(basename(dat.dir.ls), basename(gsub("_dd", "", dd.ls)))))
  if (length(ix) > 0) {
    vcat("\tInput data files missing DD and skipped:\n",
         paste(basename(dat.dir.ls)[ix], collapse = ", "), "\n")
    dat.dir.ls <- dat.dir.ls[-ix]
  }

  # ---- Read metadata files ----
  dd <- lapply(dd.ls, read.csv, strip.white = TRUE)
  names(dd) <- gsub("\\.csv", "", basename(dd.ls))

  flmd <- lapply(flmd.ls, read.csv, strip.white = TRUE)
  names(flmd) <- gsub("\\.csv", "", basename(flmd.ls))

  # Exclude input files without matching flmd
  if (length(dat.ls) > 0) {
    ix <- which(unlist(lapply(lapply(basename(dat.ls), function(x)
      unlist(lapply(flmd, function(y)
        lapply(y$fileName, function(z) grep(glob2rx(z), x))))), function(d) length(d) == 0)))
    if (length(ix) > 0) {
      vcat("\tInput files missing FLMD and skipped:\n",
           paste(basename(dat.ls)[ix], collapse = ", "), "\n")
      dat.ls <- dat.ls[-ix]
    }
  }

  # Exclude flmd files without matching input data
  flmd.na <- Filter(Negate(is.null), lapply(flmd, function(x)
    names(unlist(
      sapply(
        sapply(
          glob2rx(x$fileName), grep, basename(dat.ls)),
        function(y) which(length(y) == 0))))))
  if (length(unlist(flmd.na)) > 0) {
    flmd.na.nms <- unlist(lapply(flmd, function(x) {
      ix <- unlist(sapply(unlist(flmd.na), grep, x$fileName))
      x$fileName[ix]
    }))
    flmd <- lapply(flmd, function(x) x[-ix, ])
    vcat(paste0("FLMD files reference missing input files:\n",
                paste(flmd.na.nms, collapse = ", "), "\n"))
  }

  # ---- Check input columns against dd ----
  dat.ls.colNms <- lapply(setNames(dat.dir.ls, nm = basename(dat.dir.ls)), get_CSV_nms)
  for (i in seq_along(dat.ls.colNms)) {
    if (any(!(dat.ls.colNms[[i]] %in% dd[[i]][["colName"]]))) {
      miss <- dat.ls.colNms[[i]][!(dat.ls.colNms[[i]] %in% dd[[i]][["colName"]])]
      vcat("Data dictionary file '", basename(dd.ls[i]), "' missing columns: ", paste(miss, collapse = ", "), "\n")
    }
  }

  return(list(flmd = flmd, dd = dd))
}
