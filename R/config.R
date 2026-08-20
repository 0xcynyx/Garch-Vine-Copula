#' Configuration for the GARCH vine copula study.
#'
#' Every tunable lives here so the analysis scripts hold no magic numbers. Override any value
#' by editing this file rather than editing the functions that consume it.

#' Path to the returns workbook, one date column plus one column per market.
DATA_PATH <- Sys.getenv("GVC_DATA_PATH", "data/Data Sheet 06-10-2020.xlsx")

#' Name of the date column in the source workbook.
DATE_COLUMN <- "Date"

#' Markets analysed, in the order used for the vine structure.
MARKETS <- c(
  "Australia", "Canada", "Germany", "Japan", "UK", "USA",
  "Bangladesh", "China", "India", "Indonesia", "Pakistan", "Russia"
)

#' Confidence levels for value at risk, as tail probabilities.
VAR_ALPHAS <- c(0.01, 0.05, 0.10)

#' Rolling backtest settings, n_start is the size of the first estimation window.
BACKTEST <- list(
  n_start = 2229,
  refit_every = 10,
  refit_window = "moving",
  horizon = 1
)

#' Range of observations plotted in the backtest charts.
PLOT_WINDOW <- 2230:2429

#' Horizon in days for the out of sample volatility forecast.
FORECAST_HORIZON <- 360

#' Candidate variance models compared before the marginal is chosen.
CANDIDATE_MODELS <- c(sGARCH = "sGARCH", eGARCH = "eGARCH", gjrGARCH = "gjrGARCH")

#' Candidate innovation distributions compared for the selected variance model.
CANDIDATE_DISTRIBUTIONS <- c(
  norm = "norm", snorm = "snorm", std = "std",
  sstd = "sstd", ged = "ged", sged = "sged"
)

#' Marginal model chosen by the selection step, AR(1) EGARCH(1,1) with normal innovations.
MARGINAL <- list(
  arma_order = c(1, 0),
  variance_model = "eGARCH",
  garch_order = c(1, 1),
  distribution = "norm",
  solver = "hybrid"
)

#' Vine copula fitting options.
VINE <- list(
  type = "RVine",
  selection_criterion = "BIC",
  tree_criterion = "tau"
)

#' Colours used consistently across the value at risk charts.
VAR_COLOURS <- c(actual = "black", "0.01" = "red", "0.05" = "blue", "0.1" = "purple")
