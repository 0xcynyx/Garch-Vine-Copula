#!/usr/bin/env Rscript
# Base R checks for the data free functions, usage: Rscript tests/test-functions.R

source("R/config.R")
source("R/data.R")
source("R/descriptives.R")
source("R/garch.R")

failures <- 0

check <- function(label, actual, expected, tolerance = 1e-9) {
  # NA compares to NA as NA, so equality is decided position by position.
  ok <- if (is.numeric(actual) && is.numeric(expected)) {
    length(actual) == length(expected) &&
      all(mapply(function(x, y) {
        if (is.na(x) || is.na(y)) is.na(x) && is.na(y) else abs(x - y) < tolerance
      }, actual, expected))
  } else {
    identical(as.character(actual), as.character(expected))
  }
  ok <- isTRUE(ok)
  cat(if (ok) "PASS " else "FAIL ", label, "\n", sep = "")
  if (!ok) {
    cat("      expected: ", paste(expected, collapse = ", "), "\n", sep = "")
    cat("      actual:   ", paste(actual, collapse = ", "), "\n", sep = "")
    failures <<- failures + 1
  }
}

panel <- data.frame(a = c(1, NA, 3, 4), b = c(5, 6, NA, 8))
rownames(panel) <- as.character(as.Date("2020-01-01") + 0:3)

check("count_missing finds every gap", count_missing(panel), c(1, 1))

filled <- impute_missing(panel)
check("interpolation fills interior gaps", count_missing(filled), c(0, 0))
check("interpolated value is the midpoint", filled$a[2], 2)
check("interpolation is linear across a wider gap", filled$b[3], 7)
check("observed values are untouched", filled$a[4], 4)
check("row names survive imputation", rownames(filled)[1], "2020-01-01")

check(
  "lower BIC wins even when negative",
  rank_by_bic(c("sGARCH", "eGARCH", "gjrGARCH"), c(-6.43, -6.34, -6.46))$candidate[1],
  "gjrGARCH"
)
check(
  "ranking is ascending",
  rank_by_bic(c("a", "b", "c"), c(2, -1, 5))$candidate,
  c("b", "a", "c")
)
check("missing BIC does not crash the ranking", nrow(rank_by_bic(c("a", "b"), c(NA, 1))), 2)
check("bic_of returns NA for a failed fit", bic_of(NULL), NA_real_)

check("backtest dates slice the requested window", as.character(backtest_dates(filled, 2:3)),
      c("2020-01-02", "2020-01-03"))

correlations <- correlation_matrix(data.frame(x = 1:10, y = c(2:10, 20)))
check("correlation diagonal is one", diag(correlations), c(1, 1))
check("correlation is rounded for reporting", correlations[1, 2], round(correlations[1, 2], 2))

check("config lists twelve markets", length(MARKETS), 12)
check("config lists three confidence levels", VAR_ALPHAS, c(0.01, 0.05, 0.10))
check("marginal is AR(1) EGARCH(1,1)", MARGINAL$variance_model, "eGARCH")

cat("\n", if (failures == 0) "all checks passed" else paste(failures, "checks failed"), "\n", sep = "")
quit(status = if (failures == 0) 0 else 1)
