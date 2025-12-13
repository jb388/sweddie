#' Build core SWEDDIE database
#'
#' @param DIR parent directory in which SWEDDIE 'database' directory is stored
#' @param write_report should report of build be written to a file?
#' @param verbose should output be printed to console?
#' @return list
#' @keywords internal
#' @description returns SWEDDIE core database object with meta, site, and plot tables
compile_core <- function(DIR = "~/eco-warm/data/sweddie", write_report = TRUE, verbose = TRUE) {

  # Constants
  DB_DIR <- "database"
  S_DIR <- "siteData"
  LIST_FILE <- "coreData.rda"
  TIMESTAMP <- format(Sys.time(), "%y%m%d-%H%M")

  # configure logging for this run
  .sweddie_log_opts$verbose <- verbose
  .sweddie_log_opts$append  <- TRUE

  # Set output file
  outfile <- ""
  if (write_report) {
    outfile <- file.path(DIR, DB_DIR, paste0("logs/coreLog", "_", TIMESTAMP, ".txt"))
    invisible(file.create(outfile))
    .sweddie_log_opts$file <- outfile
  } else {
    .sweddie_log_opts$file <- ""  # console only
  }

  # Start writing in the output file
  vcat("SWEDDIE Compilation Log \n",
       "\n", as.character(Sys.time()),
       "\n", rep("-", 15), "\n")

  vcat("\n\nCompiling data files in", S_DIR, "\n", rep("-", 30), "\n")

  data_dirs <- list.dirs(file.path(DIR, S_DIR), full.names = TRUE, recursive = FALSE)
  if (!length(data_dirs)) {
    vcat("No data directories found!\n")
    return(NULL)
  }

  # ensure EOL carriage return present
  invisible(crAdd(file.path(DIR, S_DIR)))

  vcat("Compiling and checking core data...\n\n")
  #   pb <- txtProgressBar(min = 0, max = length(data_dirs), style = 3)
  # }
  #
  # check if previous database object exists in database directory, and only update file if new data exisit
  if (file.exists(file.path(DIR, DB_DIR, LIST_FILE))) {

    # load existing database
    load(file.path(dataset_directory, DB_DIR, LIST_FILE)) # obj "coreDat"

    # convert to character and coerce to list of data frames
    coreDat_chr <- lapplydf(lapply(coreData, function(x) lapply(x, as.character)))

    # remove old version
    rm(coreDat)

    # Split each table by entry_name
    coreDat_old <- lapply(coreDat_chr, function(x) split(x, x$exp_name))
  } else {
    database <- setNames(vector(mode = "list", length = length(data_dirs)), nm = basename(data_dirs))
  }

  # compile new templates and check against existing data
  for (d in seq_along(database)) {
    cat(names(database[d]), "\n")
    # get expName
    expName <- names(database[d])

    vcat("\n", expName, "\n")

    # get tables
    tbls <- list.files(data_dirs[d], full.names = TRUE)

    # read files
    datIn <- lapply(
      setNames(tbls, nm = sub("\\..*", "", basename(tbls))),
      function(f) {
        df <- read.csv(f, stringsAsFactors = FALSE, strip.white = TRUE)

        # Trim leading/trailing spaces in all character columns
        df[] <- lapply(df, function(x) {
          if (is.character(x)) trimws(x) else x
        })

        # Remove rows where all values are NA or empty string
        df <- df[rowSums(is.na(df) | df == "") != ncol(df), ]

        df
      }
    )

    # check data fidelity against template
    err <- 0
    for (i in seq_along(datIn)) {

      # check column names
      err <- checkColNms(names(datIn)[i], datIn, err, file = outfile)

      # check data types

      # check data values
    }

    # bind to database
    if (err == 0) database[[d]] <- datIn
  }
  database
}
