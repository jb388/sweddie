#' Harmonization function for ingesting raw data into SWEDDIE
#'
#' @param DIR local directory for SWEDDIE
#' @param expName name of experiment
#' @param path.dat.csv path to raw data file
#' @param path.dd.csv path to data dictionary file of raw data
#' @param append.flmd should new rows be added to existing FLMD file?
#' @param compress should data files be compressed (TRUE by default)
#' @param ... used internally for optional arguments passed to function
#' @details interactive function for harmonizing raw data which outputs dat files and their dd files, and optionally updates FLMD
#' @importFrom stats na.omit
#' @importFrom utils menu write.csv
#' @importFrom lubridate ymd_hms
#' @export
ingestDat <- function(DIR = "~/sweddie_db", expName, path.dat.csv, path.dd.csv, append.flmd, compress = TRUE, ...) {

  message(paste0("Experiment name: ", expName, "\n", "File name: ", basename(path.dat.csv), "\n\n"))

  # define canonical names
  canonical_vars <- list(
    ix.sit = "sit_name",
    ix.plt = "plt_name",
    ix.rid = "rep_name",
    ix.dpt = "depth",
    ix.tim = "date",
    ix.dat = "data",
    ix.rep = "rep_num",
    ix.var = "variance"
  )

  # read raw data files
  dat <- read_csv_cmp(path.dat.csv, strip.white = TRUE, check.names = FALSE, as.is = TRUE)
  dd <- read_csv_cmp(path.dd.csv, strip.white = TRUE, check.names = FALSE, as.is = TRUE)

  # get metadata
  sweddie_meta <- compile_meta(verbose = FALSE, EOL_err = TRUE)

  # check data orientation
  hzn.vrt <- menu(
    c("horizontal", "vertical"),
    title = paste0("How are data oriented? i.e., are data oriented in rows (horizontal), e.g., ID column/s with subsequent measurement columns, or in columns (vertical), e.g., each column represents data from a different sensor, different depths, or plot?"))
  if (hzn.vrt == "vertical") {
    message("Unfortunately this function does not accept vertically oriented data. Please reorient and try again.\n")
    return(NULL)
  }

  # function to get and validate indices
  get_valid_indices <- function(df, colType) {

    # print indices
    cat(paste0(1:length(names(df)), ": ", names(df)), sep = "\n")

    while (TRUE) {

      # Prompt the user to enter row indices or 'cancel'
      cat(paste0("Specify ", "'", colType, "'", " column/s by index # (comma separated) or enter '0' to cancel: "))
      input <- readline()

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
      if (all(!is.na(ix)) && all(ix >= 1) && all(ix <= ncol(df))) {
        return(ix)
      } else {
        cat("Error: Invalid indices. Please ensure the indices are numeric and within the range (1 to ", ncol(df), ").\n")
      }
    }
  }

  # print columns names & prompt for req. col indices
  ## data col/s
  ix.dat <- get_valid_indices(dat, "data")
  dat.nms.in <- names(dat)[ix.dat]

  # site
  sit_sam <- menu(
    c("yes", "no"),
    title = paste0("Do all data in this file originate from the same site?"))
  if (sit_sam == 2) {
    ix.sit <- get_valid_indices(dat, "site ID")
    if (length(ix.sit) != 1) {
      stop("Exactly one site ID column is required. Please check data\n")
    }
  } else {
    ix.sit <- NULL
  }

  # get date/time col
  ix.tim <- which(names(dat) == dd$colName[which(dd$dataType == "date")])
  if (length(ix.tim) > 1) {
    message(paste0("Data dictionary (dd) file denotes more than one columns with dataType = 'date' \n but only one is allowed.\n"))
    tim.sel <- menu(
      names(dat)[ix.tim],
      title = cat("\nWhich date column would you like to use? ")
    )
    ix.tim <- names(dat)[ix.tim[tim.sel]]
  }
  if (length(ix.tim) == 0) {
    message(paste0("No columns with dataType = 'date' found in dd file.\n"))
    ix.tim <- get_valid_indices(dat, "date/timestamp")
    new.tim <- TRUE
    if (length(ix.tim) > 1) {
      message(paste0("Data dictionary (dd) file denotes more than one columns with dataType = 'date' \n but only one is allowed. Using ", ix.tim[1], ", please update dd file\n"))
      ix.tim <- ix.tim[1]
    }
    if (length(ix.tim) == 0) {
      stop(paste0("Data cannot be ingested as no date/timestamp column supplied.\n"))
    }
  }

  # get unique identifiers
  ix.plt <- which(names(dat) == "plt_name")
  if (length(ix.plt) != 1) {
    message("Column 'plt_name' missing from data file.\n")
    ix <- menu(
      c("yes", "no"),
      title = paste0("Were these data collected at the site level?"))
    if (ix == 1) {
      ix.plt <- NULL
    } else {
      ix.plt <- get_valid_indices(dat, "unique spatial identifier")
      plt.nms <- names(dat)[ix.plt]
      for (i in seq_along(ix.plt)) {
        j <- menu(names(sweddie_core[[expName]]$plot), cat(paste0("\nWhich plot table column matches input data column '", plt.nms[i], "'?\n")))
        plt.vls <- sweddie_core[[expName]]$plot[[j]]
        dat.vls <- unlist(dat[ix.plt[i]])
        diffs <- setdiff(unique(dat.vls), unique(plt.vls))
        if (length(diffs) > 0) {
          cat(
            "\nPlease check input data column '", plt.nms[i], "'. Value/s '",  unlist(diffs), "' do not match values entered in plot table column '", unique(plt.vls), "'.\n"
          )
          ix.plt[i] <- 0
        }
      }
      ix.plt <- unlist(lapply(ix.plt, function(x) {x[x != 0]}))
      if (!length(ix.plt)) {
        stop("No matching spatial identifiers found")
      }
    }
  } else {
    # check plt_names against plot table
    diffs <- setdiff(
      unique(dat[[ix.plt]]),
      sweddie_core[[expName]]$plot$plt_name
    )
    if (length(diffs) > 0) {
      stop(
        paste0("The following plt_name values in the data file do not match those in the plot table: ",
        paste(diffs, collapse = ", "), "\nplt_name values: ", sweddie_core[[expName]]$plot$plt_name)
      )
    }
  }

  ## depth
  ix.dpt <- get_valid_indices(dat, "depth/height")
  if (!is.null(ix.dpt)) {
    dpt.nms <- names(dat)[ix.dpt]

    ### check for values entered as ranges
    if (any(suppressWarnings(is.na(as.numeric(dat[[ix.dpt]]))))) {
      ix <- menu(c("yes", "no"), title = paste0("\nNon-numeric values detected in column/s '", dpt.nms, "'. Are values entered as ranges, e.g., '0-10' or '10-20 cm', etc.?"))
      if (ix == 1) {
        if (length(ix.dpt) > 1) {
          warning("\nRanges not allowed with more than one depth column\n")
        }
        # Keep only digits and decimal points
        cleaned_string <- gsub("[^0-9.-]", "", dat[[ix.dpt]])

        # Split the cleaned string by hyphen and convert to numeric
        numbers <- lapply(strsplit(cleaned_string, "-"), as.numeric)

        # bind upper & lower depths to dat
        dat <- cbind(dat, depth_upper = sapply(numbers, min), depth_lower = sapply(numbers, max))
        ix.dpt <- c(ncol(dat)-1, ncol(dat))
        dpt.nms <- names(dat)[ix.dpt]
      } else {
        warning(paste0("\nPlease fix non-numeric values in depth column/s ", dpt.nms, "\n"))
      }
    }
  }

  # check for replicate IDs
  ix <- menu(
    c("yes", "no"),
    title = paste0("Do any columns contain replicate IDs?"))
  if (ix == 1) {
    ix.rid <- get_valid_indices(dat, "replicate ID")
    if (length(ix.rid) > 1) {
      warning("\nOnly one replicate ID column is permitted\n")
      return(NULL)
    }
  } else {
    ix.rid <- NULL
  }

  # check for summarized data (means/var)
  ix <- menu(
    c("yes", "no"),
    title = paste0("Do any columns contain variance estimates (standard deviation, coefficient of variation, standard error, confidence interval) or the number of replicates?"))
  if (ix == 1) {
    ix.rep <- get_valid_indices(dat, "replicates")
    if (length(ix.rep) > 1) {
      warning("\nOnly one replicate number column is permitted\n")
      return(NULL)
    }
    ix.var <- get_valid_indices(dat, "variance")
    vcl.nms <- names(dat)[ix.var]
    vcl.dat.nms <- setNames(vector(mode = "list", length = length(ix.var)), nm = vcl.nms)
    dat.nms <- names(dat[ix.dat[which(!(ix.dat %in% ix.var))]])
    for (i in seq_along(vcl.nms)) {
      ix <- menu(
        dat.nms.in,
        title = cat("\nWhich data column corresponds to variance column '", vcl.nms[i], "'?"))
      vcl.dat.nms[[i]] <- dat.nms.in[ix]
    }
  } else {
    ix.rep <- NULL
  }

  # check for duplicate records
  ix.key <- c(
    ix.sit,
    ix.plt,
    ix.tim,
    ix.dpt,
    ix.rid
  )

  # drop NULLs automatically
  ix.key <- ix.key[!vapply(ix.key, is.null, logical(1))]
  key_names <- names(dat)[ix.key]

  message(
    "Checking uniqueness of observations using: ",
    paste(key_names, collapse = " × ")
  )

  key_df <- dat[, ix.key, drop = FALSE]

  dup <- duplicated(key_df)

  if (any(dup)) {
    dup_rows <- which(dup)

    msg <- paste0(
      "Duplicate observations detected.\n",
      "Observations must be uniquely identifiable by:\n  ",
      paste(key_names, collapse = " × "), "\n",
      "Number of duplicate rows: ", length(dup_rows), "\n",
      "First duplicate rows: ", paste(head(dup_rows, 10), collapse = ", ")
    )

    warning(msg)
  }

  # get core data
  sweddie_core <- compile_core(verbose = FALSE, write_report = FALSE, EOL_err = TRUE)

  # define data and dd directories
  DATA_DIR <- file.path(DIR, "sweddie", expName, "data")
  DD_DIR <- file.path(DIR, "sweddie", expName, "dd")

  # create data files
  dat.ls <- setNames(lapply(seq_along(dat.nms.in), function(i) {

    dat.i <- match(dat.nms.in[i], names(dat))
    if (is.na(dat.i)) {
      stop("Internal error: cannot match data variable ", dat.nms.in[i])
    }
    ix.all <- c(ix.sit = ix.sit, ix.plt = ix.plt, ix.rid = ix.rid, ix.dpt = ix.dpt, ix.tim = ix.tim, ix.dat = dat.i, ix.rep = ix.rep)
    if (exists("vcl.dat.nms")) {
      var.i <- match(names(vcl.dat.nms)[match(dat.nms.in[i], unlist(vcl.dat.nms))], names(dat))
      ix.all <- c(ix.all, ix.var = var.i)
    }

    # confirm valid indices
    ix.all <- ix.all[!is.na(ix.all)]

    # subset data and rename as needed
    dat.sub <- dat[, ix.all, drop = FALSE]
    names(dat.sub) <- canonical_vars[match(names(ix.all), names(canonical_vars))]

    return(dat.sub)
  }), nm = dat.nms.in)
  for (i in seq_along(dat.ls)) {

    nm <- paste(names(dat.ls)[i], sub("\\.csv(\\.gz)?$", "", basename(path.dat.csv)), sep = "_")
    names(dat.ls)[i] <- nm
    out_csv <- file.path(DATA_DIR, nm)

    # check for duplicates (.csv or .csv.gz)
    if (any(startsWith(list.files(DATA_DIR), nm))) {
      nm <- paste0(
        readline(
          prompt = paste0(
            "Duplicate file name detected. Please supply an alternative name for the ",
            names(dat.ls)[i], " data file\n"
          )
        ),
        ".csv"
      )
      out_csv <- file.path(DATA_DIR, nm)
    }

    if (compress) {
      gz_path <- paste0(out_csv, ".csv.gz")
      con <- gzfile(gz_path, open = "wt")
      write.csv(dat.ls[[i]], con, row.names = FALSE)
      close(con)
      names(dat.ls)[i] <- basename(gz_path)
    } else {
      csv_path <- paste0(out_csv, ".csv")
      write.csv(dat.ls[[i]], out_csv, row.names = FALSE)
      names(dat.ls)[i] <- basename(out_csv)
    }

  }

  # create dd files
  dd.ls <- lapply(seq_along(dat.ls), function(i) {

    dat_cols <- names(dat.ls[[i]])
    original_cols <- sapply(dat_cols, function(cn) {
      idx <- which(canonical_vars == cn)
      if (length(idx) == 0) return(NA_character_)
      get(names(canonical_vars)[idx])
    })
    if (length(match("data", names(original_cols))) > 1) {
      original_cols["data"] <- original_cols["data"][i]
    }
    dd_rows <- dd[unlist(original_cols, use.names = FALSE), , drop = FALSE]
    dd_rows$colName <- dat_cols

    if (is.na(dd_rows[dd_rows$colName == "date", "dataType"])) {
      dd_rows[dd_rows$colName == "date", "dataType"] <- "date"
    }
    dd_rows
  })
  for (i in seq_along(dd.ls)) {

    nm <- paste0(sub("\\.csv(\\.gz)?$", "", names(dat.ls)[i]), "_dd.csv")
    if (any(grepl(nm, list.files(file.path(DD_DIR))))) {
      nm <- paste0(readline(prompt = paste0("Duplicate file name detected. Please supply an alternative name for the ", names(dat.ls[i]), " dd file", "\n")), ".csv")
    }
    write.csv(
      dd.ls[[i]],
      file = file.path(DD_DIR, nm),
      row.names = FALSE
    )
  }

  # update flmd
  if (append.flmd) {
    lapply(seq_along(dat.ls), function(i) {
      flmd_helper(
        expName = expName,
        dataFileName = sub("\\.csv(\\.gz)?$", "", names(dat.ls)[i]),
        dateColName = names(dat)[ix.tim],
        append = append.flmd,
        write_out = TRUE, ...)
    })
  }
}
