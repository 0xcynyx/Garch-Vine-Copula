#' Marginal model specification, selection, and fitting.

#' Build a univariate GARCH specification.
#'
#' @param variance_model Variance model name accepted by rugarch, for example eGARCH.
#' @param distribution Innovation distribution name, for example norm or sstd.
#' @param arma_order Mean model ARMA order.
#' @param garch_order Variance model GARCH order.
build_spec <- function(variance_model = MARGINAL$variance_model,
                       distribution = MARGINAL$distribution,
                       arma_order = MARGINAL$arma_order,
                       garch_order = MARGINAL$garch_order) {
  rugarch::ugarchspec(
    mean.model = list(armaOrder = arma_order),
    variance.model = list(model = variance_model, garchOrder = garch_order),
    distribution.model = distribution
  )
}

#' Fit one series, returning NULL instead of aborting when the optimiser fails.
#'
#' @param series Numeric vector of returns.
#' @param spec A ugarchspec object.
#' @param solver Solver passed to rugarch.
fit_series <- function(series, spec = build_spec(), solver = MARGINAL$solver) {
  tryCatch(
    rugarch::ugarchfit(data = series, spec = spec, solver = solver),
    error = function(condition) {
      warning("Fit failed: ", conditionMessage(condition), call. = FALSE)
      NULL
    }
  )
}

#' Bayesian information criterion of a fitted model.
#'
#' @param fit A ugarchfit object or NULL.
bic_of <- function(fit) {
  if (is.null(fit)) return(NA_real_)
  unname(rugarch::infocriteria(fit)["Bayes", 1])
}

#' Compare candidate variance models on one series.
#'
#' @param series Numeric vector of returns.
#' @param models Named character vector of variance model names.
#' @return A data frame ordered by BIC, lowest first.
compare_variance_models <- function(series, models = CANDIDATE_MODELS) {
  scores <- vapply(models, function(model) bic_of(fit_series(series, build_spec(variance_model = model))), numeric(1))
  rank_by_bic(names(models), scores)
}

#' Compare candidate innovation distributions for one variance model.
#'
#' @param series Numeric vector of returns.
#' @param variance_model Variance model held fixed during the comparison.
#' @param distributions Named character vector of distribution names.
#' @return A data frame ordered by BIC, lowest first.
compare_distributions <- function(series, variance_model = MARGINAL$variance_model,
                                  distributions = CANDIDATE_DISTRIBUTIONS) {
  scores <- vapply(distributions, function(distribution) {
    bic_of(fit_series(series, build_spec(variance_model = variance_model, distribution = distribution)))
  }, numeric(1))
  rank_by_bic(names(distributions), scores)
}

#' Order candidates by BIC.
#'
#' Lower BIC is better whatever its sign, so a negative score of -6.46 beats -6.34. The original
#' script picked the value nearest zero instead, which is why its chosen marginal differs from
#' the one this ranking selects. See the README section on model selection.
#'
#' @param candidates Candidate names.
#' @param scores Matching BIC values.
rank_by_bic <- function(candidates, scores) {
  frame <- data.frame(candidate = candidates, bic = unname(scores), stringsAsFactors = FALSE)
  frame[order(frame$bic), , drop = FALSE]
}

#' Fit the marginal model to every market.
#'
#' @param returns Returns data frame.
#' @param spec A ugarchspec object applied to all series.
#' @param markets Columns to fit.
#' @return A named list of ugarchfit objects.
fit_all_markets <- function(returns, spec = build_spec(), markets = MARKETS) {
  fits <- lapply(markets, function(market) {
    message("Fitting ", market)
    fit_series(returns[[market]], spec)
  })
  stats::setNames(fits, markets)
}

#' Collect BIC for every fitted market.
#'
#' @param fits Named list of fits.
bic_table <- function(fits) {
  data.frame(market = names(fits), bic = vapply(fits, bic_of, numeric(1)), row.names = NULL)
}

#' Forecast conditional volatility ahead for one fitted market.
#'
#' @param fit A ugarchfit object.
#' @param horizon Days ahead.
forecast_volatility <- function(fit, horizon = FORECAST_HORIZON) {
  rugarch::ugarchforecast(fit, n.ahead = horizon)
}
