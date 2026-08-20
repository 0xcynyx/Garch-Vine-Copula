# Loading and preparing the returns panel.

# Read the workbook and index it by date, keeping only the configured markets.
load_returns <- function(path = DATA_PATH, date_column = DATE_COLUMN, markets = MARKETS) {
  if (!file.exists(path)) {
    stop("Data file not found: ", path, "\nSet GVC_DATA_PATH or place the workbook there.", call. = FALSE)
  }
  raw <- readxl::read_excel(path)
  missing_columns <- setdiff(c(date_column, markets), names(raw))
  if (length(missing_columns) > 0) {
    stop("Workbook is missing columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  frame <- as.data.frame(raw[, c(date_column, markets), drop = FALSE])
  rownames(frame) <- as.character(frame[[date_column]])
  frame[[date_column]] <- NULL
  frame
}

# Count missing values per column.
count_missing <- function(returns) {
  vapply(returns, function(column) sum(is.na(column)), integer(1))
}

# Fill gaps by linear interpolation, which suits daily financial series.
impute_missing <- function(returns) {
  as.data.frame(lapply(returns, function(column) {
    if (any(is.na(column))) zoo::na.approx(column, na.rm = FALSE) else column
  }), row.names = rownames(returns))
}

# Prepare the panel end to end and report what was imputed.
prepare_returns <- function(path = DATA_PATH, verbose = TRUE) {
  returns <- load_returns(path)
  gaps <- count_missing(returns)
  if (verbose && any(gaps > 0)) {
    message("Imputed missing values: ", paste(names(gaps)[gaps > 0], gaps[gaps > 0], sep = "=", collapse = ", "))
  }
  filled <- impute_missing(returns)
  remaining <- sum(count_missing(filled))
  if (remaining > 0) {
    warning("Still ", remaining, " missing values after interpolation, check the series edges.", call. = FALSE)
  }
  filled
}

# Dates for the plotted backtest window.
backtest_dates <- function(returns, window = PLOT_WINDOW) {
  as.Date(rownames(returns))[window]
}
