# Marginal model specification, selection, and fitting.

# Build a univariate GARCH specification from the configured or supplied parts.
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

# Fit one series, returning NULL instead of aborting when the optimiser fails.
fit_series <- function(series, spec = build_spec(), solver = MARGINAL$solver) {
  tryCatch(
    rugarch::ugarchfit(data = series, spec = spec, solver = solver),
    error = function(condition) {
      warning("Fit failed: ", conditionMessage(condition), call. = FALSE)
      NULL
    }
  )
}

# Bayesian information criterion of a fitted model, NA when the fit failed.
bic_of <- function(fit) {
  if (is.null(fit)) return(NA_real_)
  unname(rugarch::infocriteria(fit)["Bayes", 1])
}

# Compare candidate variance models on one series.
compare_variance_models <- function(series, models = CANDIDATE_MODELS) {
  scores <- vapply(models, function(model) bic_of(fit_series(series, build_spec(variance_model = model))), numeric(1))
  rank_by_bic(names(models), scores)
}

# Compare candidate innovation distributions with the variance model held fixed.
compare_distributions <- function(series, variance_model = MARGINAL$variance_model,
                                  distributions = CANDIDATE_DISTRIBUTIONS) {
  scores <- vapply(distributions, function(distribution) {
    bic_of(fit_series(series, build_spec(variance_model = variance_model, distribution = distribution)))
  }, numeric(1))
  rank_by_bic(names(distributions), scores)
}

# Order candidates ascending, since lower BIC is better whatever its sign.
rank_by_bic <- function(candidates, scores) {
  frame <- data.frame(candidate = candidates, bic = unname(scores), stringsAsFactors = FALSE)
  frame[order(frame$bic), , drop = FALSE]
}

# Fit the marginal model to every market, replacing the twelve copied blocks.
fit_all_markets <- function(returns, spec = build_spec(), markets = MARKETS) {
  fits <- lapply(markets, function(market) {
    message("Fitting ", market)
    fit_series(returns[[market]], spec)
  })
  stats::setNames(fits, markets)
}

# Collect BIC for every fitted market.
bic_table <- function(fits) {
  data.frame(market = names(fits), bic = vapply(fits, bic_of, numeric(1)), row.names = NULL)
}

# Forecast conditional volatility ahead for one fitted market.
forecast_volatility <- function(fit, horizon = FORECAST_HORIZON) {
  rugarch::ugarchforecast(fit, n.ahead = horizon)
}
