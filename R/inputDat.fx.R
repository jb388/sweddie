#' Harmonization function for ingesting raw data into SWEDDIE
#'
#' @param expName name of experiment
#' @param path.dat.csv path to raw data file
#' @param path.dd.csv path to data dictionary file of raw data
#' @param append.flmd should new rows be added to existing FLMD file?
#' @details interactive function for harmonizing raw data which outputs dat files and their dd files, and optionally updates FLMD
#' @export
inputDat.fx <- function(expName, path.dat.csv, path.dd.csv, append.flmd, ...) {

  # define allowable cols
  allowable_cols <- c("date", "depth", "depth_lower", "depth_upper", "variable", "variance", "replicates", "plt_name")

  # read raw data files
  dat <- read.csv(path.dat.csv, strip.white = TRUE, check.names = FALSE, as.is = TRUE)
  dd <- read.csv(path.dd.csv, strip.white = TRUE, check.names = FALSE, as.is = TRUE)

  # check data orientation
  hzn.vrt <- menu(
    c("horizontal", "vertical"),
    title = paste0("How are data oriented? i.e., are data oriented in rows (horizontal), e.g., ID column/s with subsequent measurement columns, or in columns (vertical), e.g., each column represents data from a different sensor, different depths, or plot?"))
  # if (hzn.vrt == "vertical") {
  #
  # }

  # function to get and validate indices
  get_valid_indices <- function(df, colType) {

    # print indices
    vcat(paste0(1:length(names(df)), ": ", names(df)), sep = "\n")

    while (TRUE) {

      # Prompt the user to enter row indices or 'cancel'
      vcat(paste0("Specify ", "'", colType, "'", " column/s by index # (comma separated) or enter '0' to cancel: "))
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

  # get flmd_dd.ls
  flmd_dd.ls <- compile_meta(verbose = FALSE)

  # print columns names & prompt for req. col indices
  ## data col/s
  ix.dat <- get_valid_indices(dat, "data")
  dat.nms.in <- names(dat)[ix.dat]
  dat.nms <- vector(mode = "list", length = length(dat.nms.in))
  for (i in seq_along(dat.nms)) {
    varName_opts <- unique(unlist(lapply(lapply(flmd_dd.ls, "[[", "flmd"), function(x) lapply(x, "[[", "varName"))))
    sel <- menu(varName_opts, title = paste0("Does the data in column '", dat.nms.in[i], "' match one of the following variable names? (Enter '0' if none are appropriate)"))
    if (sel != 0) {
      dat.nms[i] <- varName_opts[sel]
    } else {
      dat.nms[i] <- readline(prompt = paste0("Please describe the variable in data column ", dat.nms.in[i], "\n"))
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

  # check for summarized data (means/var)
  ix <- menu(
    c("yes", "no"),
    title = paste0("Do any columns contain replicates or variance estimates (standard deviation, coefficient of variation, standard error, confidence interval)?"))
  if (ix == 1) {
    ix.rep <- get_valid_indices(dat, "replicates")
    if (length(ix.rep) > 1) {
      cat("\nOnly one replicate column is permitted\n")
    }
    ix.var <- get_valid_indices(dat, "variance")
    vcl.nms <- names(dat)[ix.var]
    vcl.dat.nms <- setNames(vector(mode = "list", length = length(ix.var)), nm = vcl.nms)
    dat.nms <- names(dat[ix.dat[which(!(ix.dat %in% ix.var))]])
    for (i in seq_along(vcl.nms)) {
      ix <- menu(
        dat.nms,
        title = cat("\nWhich data column corresponds to variance column '", vcl.nms[i], "'?"))
      vcl.dat.nms[[i]] <- dat.nms[ix]
    }
  }

  # get date/time col
  ix.tim <- which(dd$dataType == "date")
  if (length(ix.tim) > 1) {
    cat("Data dictionary (dd) file denotes more than one columns with dataType = 'date' \n but only one is allowed. Please update data and dd files\n")
  } else {
    ix <- menu(
      c("yes", "no"),
      title = paste0("Is '", dd$colName[ix.tim], "' the primary date/time column?"))
    if (ix != 1) {
      cat("\nPlease check data and data dictionary files\n")
    }
  }

  # check for database object
  if (!exists("database")) {
    database <- tryCatch(coreData.fx(verbose = FALSE))
  }

  # get plt_name column
  ix.plt <- which(names(dat) == "plt_name")
  if (length(ix.plt) == 0) {
    cat("Required column 'plt_name' missing from data file\n")
    ix <- menu(
      c("plot", "site"),
      title = paste0("Were these data collected at the site or plot level?"))
    if (ix == 1) {
      cat("\nColumn 'plt_name' not detected in input data. We can try to match the plot names entered in the 'plot' table with a unique combination of identifying columns in the input data.\n")
      iix <- menu(c("yes", "no"), title = "\nDo you want to try this?\n")
      if (iix == 1) {
        ix.plt <- get_valid_indices(dat, "unique plot identifier")
        plt.nms <- names(dat)[ix.plt]
        for (i in seq_along(plt.nms)) {
          j <- menu(names(database[[expName]]$plot), cat(paste0("\nWhich plot table column matches input data column '", plt.nms[i], "'?\n")))
          plt.vls <- database[[expName]]$plot[[j]]
          dat.vls <- unlist(dat[ix.plt[i]])
          mch <- unique(dat.vls) %in% unique(plt.vls)
          if (!all(mch)) {
            cat("\nPlease check input data column '", plt.nms[i], "'. Value/s '",  unique(dat.vls)[which(!mch)], "' do not match values entered in plot table column '", plt.nms[i], "'.\n")
          }
        }
      }
      cat("\nPlease ensure column 'plt_name' is present in data and data dictionary files\n")
      return(NULL)
    } else {
      ix.plt <- NULL
    }
  }

  # check plt_names against plot table
  plt.nms <- unlist(unique(dat[ix.plt])) %in% database[[expName]]$plot$plt_name
  if (any(!plt.nms)) {
    cat("\nThe following plt_name values do not match entries in the plot: ", unlist(unique(dat[ix.plt]), use.names = FALSE)[which(!plt.nms)], "\nAllowable values: ", database[[expName]]$plot$plt_name)
  }

  # define data and metadata directories
  DATA_DIR <- path.expand(file.path("~/eco-warm/data/experiments", expName, "input_data"))
  META_DIR <- path.expand(file.path("~/eco-warm/data/experiments", expName, "meta"))

  # create data files
  dat.ls <- setNames(lapply(seq_along(dat.nms), function(i) {
    dat.i <- match(dat.nms.in[i], names(dat))
    ix <- c(ix.tim, ix.plt, ix.dpt, dat.i)
    if (exists("vcl.dat.nms")) {
      var.i <- match(names(vcl.dat.nms)[match(dat.nms[i], unlist(vcl.dat.nms))], names(dat))
      ix <- c(ix, var.i)
    }
    dat[ , na.omit(ix)]
  }), nm = dat.nms)
  for (i in seq_along(dat.ls)) {
    nm <- paste0(names(dat.ls[i]), ".csv")
    if (any(grepl(nm, list.files(file.path(DATA_DIR))))) {
      nm <- paste0(readline(prompt = paste0("Duplicate file name detected. Please supply an alternative name for the ", names(dat.ls[i]), " data file", "\n")), ".csv")
    }
    write.csv(
      dat.ls[[i]],
      file = file.path(DATA_DIR, nm),
      row.names = FALSE
    )
  }

  # create dd files
  dd.ls <- setNames(lapply(seq_along(dat.ls), function(i) {
    dd[match(names(dat.ls[[i]]), dd$colName), ]
  }), nm = paste0(names(dat.ls), "_dd"))
  for (i in seq_along(dd.ls)) {
    nm <- paste0(names(dd.ls[i]), ".csv")
    if (any(grepl(nm, list.files(file.path(META_DIR))))) {
      nm <- paste0(readline(prompt = paste0("Duplicate file name detected. Please supply an alternative name for the ", names(dat.ls[i]), " dd file", "\n")), ".csv")
    }
    write.csv(
      dd.ls[[i]],
      file = file.path(META_DIR, nm),
      row.names = FALSE
    )
  }

  # update flmd
  if (append.flmd) {
    lapply(seq_along(dat.ls), function(i) {
      flmd_helper(
        expName = expName,
        dataFileName = names(dat.ls[i]),
        dateColName = names(dat[ix.tim]),
        append = append.flmd,
        write_out = TRUE, ...)
    })
  }
}
