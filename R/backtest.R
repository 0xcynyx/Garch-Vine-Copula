#' Rolling value at risk backtests and their test statistics.

#' Run one rolling backtest for a single series and tail probability.
#'
#' @param series Numeric vector fed to the rolling estimator.
#' @param alpha Tail probability, for example 0.01.
#' @param spec A ugarchspec object.
#' @param settings List of rolling window settings.
roll_var <- function(series, alpha, spec = build_spec(), settings = BACKTEST) {
  tryCatch(
    rugarch::ugarchroll(
      spec,
      data = series,
      n.ahead = settings$horizon,
      n.start = settings$n_start,
      refit.window = settings$refit_window,
      refit.every = settings$refit_every,
      calculate.VaR = TRUE,
      VaR.alpha = alpha
    ),
    error = function(condition) {
      warning("Roll failed at alpha ", alpha, ": ", conditionMessage(condition), call. = FALSE)
      NULL
    }
  )
}

#' Backtest every market at every tail probability.
#'
#' This replaces the repeated per market blocks in the original script, so adding a market or a
#' confidence level is a change to the configuration rather than another copy of the code.
#'
#' @param panel Data frame whose columns are the series to backtest.
#' @param spec A ugarchspec object.
#' @param alphas Tail probabilities.
#' @param markets Columns to process.
#' @return A nested list indexed by market then by alpha as a character key.
backtest_all <- function(panel, spec = build_spec(), alphas = VAR_ALPHAS, markets = names(panel)) {
  results <- lapply(markets, function(market) {
    message("Backtesting ", market)
    per_alpha <- lapply(alphas, function(alpha) roll_var(panel[[market]], alpha, spec))
    stats::setNames(per_alpha, as.character(alphas))
  })
  stats::setNames(results, markets)
}

#' Extract the coverage test statistics from one rolling object.
#'
#' Actual percentage is the violation rate, LRuc is the unconditional coverage statistic, and
#' LRcc is the conditional coverage statistic.
#'
#' @param roll A ugarchroll object or NULL.
#' @param alpha The tail probability the roll was computed at.
var_statistics <- function(roll, alpha) {
  if (is.null(roll)) {
    return(data.frame(alpha = alpha, expected = NA_real_, actual = NA_real_, lr_uc = NA_real_, lr_cc = NA_real_))
  }
  report <- utils::capture.output(rugarch::report(roll, type = "VaR", VaR.alpha = alpha))
  numeric_from <- function(pattern) {
    line <- grep(pattern, report, value = TRUE)[1]
    if (is.na(line)) return(NA_real_)
    found <- regmatches(line, gregexpr("-?[0-9]+\\.?[0-9]*", line))[[1]]
    if (length(found) == 0) NA_real_ else as.numeric(utils::tail(found, 1))
  }
  data.frame(
    alpha = alpha,
    expected = numeric_from("Expected Exceed"),
    actual = numeric_from("Actual VaR Exceed"),
    lr_uc = numeric_from("LR.uc Statistic"),
    lr_cc = numeric_from("LR.cc Statistic")
  )
}

#' Flatten nested backtest results into one reporting table.
#'
#' @param results Output of backtest_all.
#' @return A data frame with one row per market and alpha.
backtest_table <- function(results) {
  rows <- lapply(names(results), function(market) {
    per_alpha <- results[[market]]
    frames <- lapply(names(per_alpha), function(key) {
      frame <- var_statistics(per_alpha[[key]], as.numeric(key))
      cbind(market = market, frame)
    })
    do.call(rbind, frames)
  })
  do.call(rbind, rows)
}

#' Print the native rugarch reports, matching the original script's console output.
#'
#' @param results Output of backtest_all.
print_var_reports <- function(results) {
  for (market in names(results)) {
    cat("\n==== ", market, " ====\n", sep = "")
    for (key in names(results[[market]])) {
      roll <- results[[market]][[key]]
      if (is.null(roll)) next
      cat("-- alpha ", key, "\n", sep = "")
      rugarch::report(roll, type = "VaR", VaR.alpha = as.numeric(key))
    }
  }
  invisible(NULL)
}
