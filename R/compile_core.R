#' Build core SWEDDIE database
#'
#' @param DIR parent directory in which SWEDDIE 'database' directory is stored
#' @param write_report logical; should report of build be written to a file?
#' @param verbose logical
#' @param EOL_err set to true if you encounter an end of line (EOL) error
#' @return list
#' @export
#' @description returns SWEDDIE core database object with experiment, site, and plot tables
#' @importFrom stats setNames
#' @importFrom utils read.csv
compile_core <- function (DIR = "~/sweddie_db", write_report = FALSE, verbose = FALSE, EOL_err = FALSE) {

  # Constants
  DB_DIR <- file.path(DIR, "sweddie")
  TIMESTAMP <- format(Sys.time(), "%y%m%d-%H%M")

  # Set output file
  if (write_report) {
    outfile <- file.path(file.path(DIR, "build_logs"), paste0("coreLog_", TIMESTAMP, ".txt"))
    invisible(file.create(outfile))
    .sweddie_log_opts$file <- outfile
  }

  # configure logging for this run
  .sweddie_log_opts$verbose <- verbose

  # Start writing in the output file
  vcat("SWEDDIE Compilation Log \n",
       "\n", as.character(Sys.time()),
       "\n", rep("-", 15), "\n")

  vcat("\n\nCompiling core data files in", DB_DIR,  "\n", rep("-", 30), "\n")

  data_dirs <- list.dirs(DB_DIR, full.names = TRUE, recursive = FALSE)
  if (!length(data_dirs)) {
    vcat("No data directories found!\n")
    return(NULL)
  }

  # ensure EOL carriage return present
  if (EOL_err) invisible(cr_add(DB_DIR))

  vcat("Compiling and checking core data...\n\n")

  # define database
  database <- setNames(vector(mode = "list", length = length(data_dirs)), nm = basename(data_dirs))

  # compile new templates and check against existing data
  for (d in seq_along(database)) {

    # get expName
    expName <- names(database[d])
    vcat("\n", expName, "\n")

    # get tables
    tbls <- list.files(data_dirs[d], full.names = TRUE)
    core_tbls <- tbls[grepl(".csv", tbls)]
    core_tbls <- core_tbls[!grepl("flmd", core_tbls)]

    # read files
    datIn <- lapply(
      setNames(core_tbls, nm = sub("\\..*", "", basename(core_tbls))),
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
      err <- check_col_nms(names(datIn)[i], datIn, err)

      # check data types

      # check data values
    }

    # bind to database
    if (err == 0) database[[d]] <- datIn
  }
  database
}
