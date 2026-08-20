# Descriptive statistics, normality tests, and correlation matrices.

# Summary statistics plus a Jarque Bera statistic for every series.
describe_returns <- function(returns) {
  statistics <- list(
    mean = mean, min = min, max = max, sd = stats::sd,
    skewness = e1071::skewness, kurtosis = e1071::kurtosis
  )
  summary_frame <- as.data.frame(lapply(statistics, function(fn) vapply(returns, fn, numeric(1))))
  summary_frame$jarque_bera <- vapply(returns, function(column) {
    unname(tseries::jarque.bera.test(column)$statistic)
  }, numeric(1))
  summary_frame
}

# Correlation matrix for one method, rounded for reporting.
correlation_matrix <- function(returns, method = "pearson", digits = 2) {
  round(stats::cor(returns, method = method), digits)
}

# Both correlation matrices reported in the study.
correlation_report <- function(returns) {
  list(
    pearson = correlation_matrix(returns, "pearson"),
    kendall = correlation_matrix(returns, "kendall")
  )
}
