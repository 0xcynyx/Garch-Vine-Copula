#!/usr/bin/env Rscript
#' Runs the full GARCH vine copula study end to end.
#'
#' Usage: Rscript run_analysis.R [output_dir]
#' Every parameter lives in R/config.R, so this file only expresses the order of the steps.

for (module in c("config", "packages", "data", "descriptives", "garch", "copula", "backtest", "plots")) {
  source(file.path("R", paste0(module, ".R")))
}
load_packages()

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) > 0) args[1] else "output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

save_table <- function(frame, name) {
  path <- file.path(output_dir, paste0(name, ".csv"))
  utils::write.csv(frame, path, row.names = TRUE)
  message("Wrote ", path)
}

message("== Step 1, prepare the returns panel")
returns <- prepare_returns()
message("Panel: ", nrow(returns), " observations across ", ncol(returns), " markets")

message("== Step 2, descriptive statistics and correlations")
save_table(describe_returns(returns), "descriptive-statistics")
correlations <- correlation_report(returns)
save_table(correlations$pearson, "correlation-pearson")
save_table(correlations$kendall, "correlation-kendall")

message("== Step 3, marginal model selection on the first market")
reference <- returns[[MARKETS[1]]]
save_table(compare_variance_models(reference), "selection-variance-models")
save_table(compare_distributions(reference), "selection-distributions")

message("== Step 4, fit the marginal model to every market")
spec <- build_spec()
fits <- fit_all_markets(returns, spec)
save_table(bic_table(fits), "marginal-bic")

message("== Step 5, volatility forecast for the reference market")
reference_forecast <- forecast_volatility(fits[[MARKETS[1]]])
print(reference_forecast)

message("== Step 6, residuals to pseudo observations to vine copula")
residual_matrix <- standardised_residuals(fits)
pseudo_observations <- to_pseudo_observations(residual_matrix)
vines <- fit_vine_families(pseudo_observations)
for (family in names(vines)) {
  described <- describe_vine(vines[[family]])
  message(toupper(family), " BIC: ", round(described$bic, 4))
  plot_vine_structure(vines[[family]], file.path(output_dir, paste0(family, "-structure.png")))
}

message("== Step 7, value at risk backtest of the marginal models")
marginal_backtest <- backtest_all(returns, spec, VAR_ALPHAS, MARKETS)
save_table(backtest_table(marginal_backtest), "backtest-marginal")
plot_all_backtests(marginal_backtest, backtest_dates(returns), file.path(output_dir, "var-marginal"))

message("== Step 8, value at risk backtest on the copula margins")
copula_panel <- as.data.frame(pseudo_observations)
copula_backtest <- backtest_all(copula_panel, spec, VAR_ALPHAS, names(copula_panel))
save_table(backtest_table(copula_backtest), "backtest-copula")
plot_all_backtests(copula_backtest, backtest_dates(returns), file.path(output_dir, "var-copula"))

message("Done. Results in ", normalizePath(output_dir))
