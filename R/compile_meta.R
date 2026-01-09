#' Compile SWEDDIE metadata
#'
#' @param DIR local directory for SWEDDIE database
#' @param expName name of experiment to retrieve metadata from; must match standard names
#' @param write_report logical; should report be written to a file?
#' @param verbose logical
#' @return list
#' @export
#' @description returns SWEDDIE metadata object
#' @importFrom utils glob2rx
compile_meta <- function (DIR = "~/eco-warm/data", expName, verbose = TRUE, write_report = FALSE) {

  # vectorized
  if (length(expName) > 1) {
    res <- lapply(expName, function(x) {
      compile_meta(
        DIR = DIR,
        expName = x,
        verbose = verbose,
        write_report = write_report
      )
    })
    names(res) <- expName
    return(res)
  }

  # Constants
  TIMESTAMP <- format(Sys.time(), "%y%m%d-%H%M")

  if (write_report) {
    outfile <- file.path(DIR, paste0("sweddie/database/logs/coreLog", "_", TIMESTAMP, ".txt"))
    invisible(file.create(outfile))
    .sweddie_log_opts$append  <- TRUE
  } else {
    outfile <- ""
    .sweddie_log_opts$append  <- FALSE
  }

  # configure logging for this run
  .sweddie_log_opts$verbose <- verbose
  .sweddie_log_opts$file <- outfile

  # get site dir paths and variable directory names
  exp.ls <- list.dirs(file.path(DIR, "experiments"), recursive = FALSE)
  exp.dir <- exp.ls[match(expName, basename(exp.ls))]
  stopifnot(dir.exists(exp.dir))

  # start log
  vcat("\n\nCompiling metadata files in", exp.dir, "\n", rep("-", 30), "\n")

  if (length(list.files(exp.dir)) == 0) {
    vcat("\n", expName, "\n\n", "No files found in ", exp.dir)
    return(NULL)
  } else {
    dat.dir <- file.path(exp.dir, "input_data")
    stopifnot(dir.exists(dat.dir))
    dat.dir.ls <- list.files(dat.dir, full.names = TRUE)
    mta.dir <- file.path(exp.dir, "meta")
    stopifnot(dir.exists(mta.dir))
    mta.dir.ls <- list.files(mta.dir, full.names = TRUE)

    # check metadata directory
    flmd.ls <- mta.dir.ls[which(grepl("_flmd.csv", mta.dir.ls))]
    dd.ls <- mta.dir.ls[which(grepl("_dd.csv", mta.dir.ls))]

    # ensure EOL carriage return present
    invisible(cr_add(mta.dir))

    if (length(flmd.ls) == 0 | length(dd.ls) == 0) {
      return (NULL)
    }

    # check for non-standard files
    if (sum(length(flmd.ls), length(dd.ls)) != length(mta.dir.ls)) {
      ix <- which(!(basename(mta.dir.ls) %in% c(basename(flmd.ls), basename(dd.ls))))
      vcat("\t meta directory contains the following non-standard files that will be ignored:\n", basename(mta.dir.ls)[ix], "\n")
    }

    # get valid input data names from dd.ls
    dat.ls <- dat.dir.ls[which(basename(dat.dir.ls) %in% basename(gsub("_dd", "", dd.ls)))]

    # exclude dd files without matching input data
    ix <- which(is.na(match(basename(gsub("_dd", "", dd.ls)), basename(dat.dir.ls))))
    if (length(ix) > 0) {
      vcat("\tThe following data dictionary files are missing input data and will not be ingested:\n", basename(dd.ls)[ix], "\n")
      dd.ls <- dd.ls[-ix]
    }

    # report input data missing dd file
    ix <- which(is.na(match(basename(dat.dir.ls), basename(gsub("_dd", "", dd.ls)))))
    if (length(ix) > 0) {
      vcat("\tThe following input data files are missing data dictionaries and will not be ingested:\n", basename(dat.dir.ls)[ix], "\n")
      dat.dir.ls <- dat.dir.ls[-ix]
    }

    # read dd files
    dd <- lapply(dd.ls, read.csv, strip.white = TRUE)
    names(dd) <- gsub("\\.csv", "", basename(dd.ls))

    # read flmd files
    flmd <- lapply(flmd.ls, read.csv, strip.white = TRUE)
    names(flmd) <- gsub("\\.csv", "", basename(flmd.ls))

    # exclude input files without matching flmd
    if (length(dat.ls) > 0) {
      ix <- which(unlist(lapply(lapply(basename(dat.ls), function(x)
        unlist(lapply(flmd, function(y)
          lapply(y$fileName, function(z)
            grep(glob2rx(z), x))))), function(d) length(d) == 0)))
      if (length(ix) > 0) {
        vcat("\tThe following input data files are missing flmd and will not be ingested:\n", basename(dat.ls)[ix], "\n")
        dat.ls <- dat.ls[-ix]
      }
    }

    # exclude flmd files without matching input data
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
      vcat(paste0("The following input files are listed in the flmd file '", names(flmd.na.nms), ".csv', but cannot be found:\n", flmd.na.nms, "\n"))
    }

    # check input data names against dd
    dat.ls.colNms <- lapply(
      setNames(dat.dir.ls, nm = basename(dat.dir.ls)), get_CSV_nms)

    for (i in seq_along(dat.ls.colNms)) {

      if (any(!(dat.ls.colNms[[i]] %in% dd[[i]][["colName"]]))) {
        miss <- dat.ls.colNms[[i]][which(!(dat.ls.colNms[[i]] %in% dd[[i]][["colName"]]))]
        vcat("Data dictionary file '", basename(dd.ls[i]), "' missing column/s: ", miss, "\n")
      }
    }
    return(list(flmd = flmd, dd = dd))
  }
}
