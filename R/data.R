#' Loading and preparing the returns panel.

#' Read the workbook and index it by date.
#'
#' @param path Path to an xlsx file with a date column and one column per market.
#' @param date_column Name of the date column.
#' @param markets Columns to keep, in order.
#' @return A data frame of returns with dates as row names.
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

#' Count missing values per column.
#'
#' @param returns Returns data frame.
#' @return Named integer vector, one entry per column.
count_missing <- function(returns) {
  vapply(returns, function(column) sum(is.na(column)), integer(1))
}

#' Fill gaps by linear interpolation, which suits daily financial series.
#'
#' @param returns Returns data frame.
#' @return The same frame with interior gaps filled.
impute_missing <- function(returns) {
  as.data.frame(lapply(returns, function(column) {
    if (any(is.na(column))) zoo::na.approx(column, na.rm = FALSE) else column
  }), row.names = rownames(returns))
}

#' Prepare the panel end to end and report what was imputed.
#'
#' @param path Path to the workbook.
#' @param verbose Whether to print a summary of imputed values.
#' @return A clean returns data frame.
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

#' Dates for the plotted backtest window.
#'
#' @param returns Returns data frame indexed by date.
#' @param window Integer positions to keep.
#' @return A Date vector.
backtest_dates <- function(returns, window = PLOT_WINDOW) {
  as.Date(rownames(returns))[window]
}
