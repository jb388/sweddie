#' Check observation frequency
#'
#' @param df data frame
#' @param dateName name of column containing observation dates
#' @param pltName name of column with plot identifiers
#' @param depth are data depth resolved? If so, data must contain a column called "depth"
#' @return list
#' @keywords internal
#' @description returns list of median interval in seconds, days, inferred sampling frequency, etc.
#' @importFrom lubridate parse_date_time
#' @importFrom stats median
#' @importFrom dplyr case_when
ts_freq <- function(df, dateName = "Date", pltName = "plt_name", repName = NULL, depth = FALSE) {

  # validate inputs
  if (!dateName %in% names(df)) stop("Column '", dateName, "' not found in data frame.")
  if (!pltName %in% names(df)) stop("Column '", pltName, "' not found in data frame.")
  if (!is.data.frame(df)) stop("Input must be a data frame.")

  # filter to single plot
  p1 <- df[[pltName]][1]
  df.f <- df[df[[pltName]] == p1, , drop = FALSE]

  # filter to single depth (as needed)
  if (depth && "depth" %in% names(df)) {
    min_depth <- suppressWarnings(min(df.f$depth, na.rm = TRUE))
    df.f <- df.f[df.f$depth == min_depth, , drop = FALSE]
  }

  # filter to rep (as needed)
  if (!is.null(repName)) {
    r1 <- df[[repName]][1]
    df.f <- df.f[df.f[[repName]] == r1, , drop = FALSE]
  }

  # Parse and clean date column
  # Convert to POSIXct or Date robustly
  dates_raw <- df.f[[dateName]]

  # Handle different possible formats automatically
  if (!inherits(dates_raw, c("Date", "POSIXct", "POSIXt"))) {
    dates <- suppressWarnings(parse_date_time(
      dates_raw,
      orders = c("Ymd HMS", "Ymd HM", "Ymd H", "Ymd", "mdY", "dmy", "ymd"),
      tz = "UTC"
    ))
  } else {
    dates <- as.POSIXct(dates_raw)
  }

  # Drop invalid or NA dates
  dates <- sort(dates[!is.na(dates)])

  if (length(dates) < 2) {
    warning("Not enough valid dates to estimate frequency.")
  }

  # Compute intervals
  diffs <- diff(dates)
  dt <- as.numeric(median(diffs, na.rm = TRUE), units = "secs")

  # --- 5. Map to human-readable frequency -----------------------------------
  freq <- case_when(
    dt < 60 ~ "sub-minute",
    dt < 3600 ~ "minutely",
    dt < 86400 ~ "hourly",
    dt < 86400 * 7 ~ "daily",
    dt < 86400 * 31 ~ "weekly",
    dt < 86400 * 365 ~ "monthly",
    TRUE ~ "yearly"
  )

  # Return clean summary
  list(
    median_interval_days = round(dt / 86400, 3),
    inferred_frequency = freq,
    date_range = paste0(format(dates[1]), " to ", format(dates[length(dates)])),
    n_obs = length(dates)
  )
}
