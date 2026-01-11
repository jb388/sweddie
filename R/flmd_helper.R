#' Helper for creating or appending file level metadata file
#'
#' @param DIR local directory for SWEDDIE
#' @param expName name of experiment
#' @param dataFileName name of input file (must be *.csv file, do not include extension)
#' @param dateColName supply known text string for column in input data containing dates
#' @param rename should file name be overwritten, e.g., 'expName_flmd'?
#' @param append should new rows be added to existing FLMD file?
#' @param write_out should function write out a FLMD file in *.csv format?
#' @param orders optionally supply date format, e.g., %m-%d-%Y
#' @param ... used internally for optional arguments passed to function
#' @details interactive function for filling out or updating file level metadata (FLMD)
#' @importFrom utils menu read.csv write.csv
#' @export
flmd_helper <- function(DIR = "~/sweddie_db", expName, dataFileName, dateColName, rename = FALSE, append = TRUE, write_out = TRUE, orders = NULL, ...) {

  # list optionals
  optArgs <- list(...)

  # set data directory path
  DATA_DIR <- file.path(DIR, "sweddie", expName, "dat", "data")
  META_DIR <- file.path(DIR, "sweddie", expName, "dat", "meta")

  # get flmd template
  if (append) {
    files <- list.files(
      META_DIR,
      full.names = TRUE)
    flmd.s <- files[grepl("flmd", files)]
    if (length(flmd.s) == 0) {
      stop ("cannot append record: no flmd files found")
    }
    if (length(flmd.s) > 1) {
      stop ("multiple flmd files found but only one is allowed")
    }
    flmd <- read.csv(flmd.s)
  } else {
    # get template
    flmd <- read.csv(system.file("extdata", "templates", "meta", "flmd_SWEDDIE.csv", package = "sweddie"))
    flmdName <- file.path(META_DIR, paste0(expName, "_", "flmd.csv"))
  }

  # get data
  data <- read.csv(file.path(DATA_DIR, paste0(dataFileName, ".csv")))

  # get metadata
  sweddie_meta <- compile_meta(verbose = FALSE)

  # fill out flmd
  nm <- setNames(vector(length = ncol(flmd), mode = "list"), names(flmd))
  if (!is.null(optArgs[["Jeff"]])) {
    nm[["name"]] <- "Jeffrey Beem-Miller"
    nm[["email"]] <- "jbeemmiller@lbl.gov"
  }
  nm[["fileName"]] <- paste0(dataFileName, ".csv")
  if (is.null(dateColName)) {
    dateColName <- names(data)[
      sapply(data, inherits, what = c("POSIXct", "POSIXt", "Date"))
    ][1]

    if (is.na(dateColName)) {
      stop("Could not infer date column; please supply dateColName")
    }
  }
  if (!missing(orders)) {
    data[[dateColName]] <- parse_date_time(data[[dateColName]], orders = orders)
  } else {
    data[[dateColName]] <- ymd_hms(data[[dateColName]], truncated = 5)
  }
  nm[["startDate"]] <- as.character(min(data[[dateColName]], na.rm = TRUE))
  nm[["endDate"]] <- as.character(max(data[[dateColName]], na.rm = TRUE))

  # helper function for menus
  menu.fx <- function(col_i, opt = NULL) {

    if (is.null(opt)) {
      opt <- unique(flmd[[col_i]])
    }

    if (is.null(nm[[col_i]])) {
      j <- menu(
        c("yes", "no"),
        title = paste0(
          "Do you want to use a previously entered ",
          names(nm)[col_i],
          "?"
        )
      )

      if (j == 1) {
        sel <- menu(
          opt,
          "Please use one of the following options for your data (enter '0' if none are appropriate)"
        )
        if (sel != 0) {
          return(opt[sel])
        } else {
          return(readline(prompt = paste0(names(flmd)[col_i], "? ")))
        }
      } else {
        return(readline(prompt = paste0(names(flmd)[col_i], "? ")))
      }
    }

    nm[[col_i]]
  }

  for (i in seq_along(nm)) {
    if (is.null(nm[[i]])) {
      if (names(nm)[i] == "varName") {
        varName_opts <- unique(c(unlist(lapply(lapply(sweddie_meta, "[[", "flmd"), function(x) lapply(x, "[[", "varName"))),
                                 flmd$varName))
        nm[[i]] <- menu.fx(i, opt = varName_opts)
      } else {
        if (append) {
          nm[[i]] <- menu.fx(i)
        } else {
          nm[[i]] <- readline(prompt = paste0(names(flmd)[i], "? "))
        }
      }
    }
  }


  # append new row
  new_row <- as.data.frame(as.list(nm), stringsAsFactors = FALSE)

  # check for mismatches
  if (!all(names(flmd) %in% names(new_row))) {
    stop("Some flmd columns are missing from nm: ",
         paste(setdiff(names(flmd), names(new_row)), collapse = ", "))
  }
  if (any(!names(new_row) %in% names(flmd))) {
    warning("Extra fields in nm not present in flmd: ",
            paste(setdiff(names(new_row), names(flmd)), collapse = ", "))
  }

  # only recognized cols in flmd, ordered
  new_row <- new_row[, names(flmd), drop = FALSE]

  # if flmd is empty, just use new_row; else bind it
  if (nrow(flmd) == 0) {
    flmd <- new_row
  } else {
    flmd <- rbind(flmd, new_row)
  }

  if (write_out) {
    if (rename) {
      write.csv(flmd,
                file = file.path(META_DIR, paste0(expName, "_flmd.csv")),
                row.names = FALSE)
    } else {
      write.csv(flmd, file = flmdName, row.names = FALSE)
    }
  } else {
    flmd
  }
}
