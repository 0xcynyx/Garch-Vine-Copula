# GARCH Vine Copula

Modelling the dependence structure between twelve equity markets with **AR(1) EGARCH(1,1)
marginals and an R-vine copula**, then backtesting Value at Risk at the 99, 95 and 90 percent
levels.

Six developed markets (Australia, Canada, Germany, Japan, UK, USA) and six emerging ones
(Bangladesh, China, India, Indonesia, Pakistan, Russia).

## Method

The pipeline is the standard two stage copula approach. Marginals absorb each market's own
volatility dynamics, then the copula captures what is left, which is the dependence between
markets.

1. **Prepare** the returns panel, interpolating interior gaps.
2. **Describe** it: central tendency, dispersion, skewness, kurtosis, Jarque Bera, and both
   Pearson and Kendall correlation matrices.
3. **Select a marginal** by comparing sGARCH, EGARCH and GJR-GARCH, then comparing six
   innovation distributions for the winner, ranking by BIC.
4. **Fit** that marginal to all twelve markets.
5. **Transform** the standardised residuals into pseudo observations with uniform margins,
   which is what a copula needs.
6. **Fit the vine**, selecting structure and pair copula families by BIC with Kendall tau as
   the tree criterion. Both R-vine and C-vine are fitted.
7. **Backtest** Value at Risk for every market at every level, reporting the violation rate
   with unconditional and conditional coverage statistics.

## Layout

```
run_analysis.R          the whole study as an ordered sequence of steps
R/config.R              markets, confidence levels, window sizes, model specs
R/packages.R            dependency loading with an actionable error
R/data.R                loading, validation, interpolation, backtest dates
R/descriptives.R        summary statistics, normality, correlations
R/garch.R               spec building, BIC ranking, fitting across markets
R/copula.R              residuals, pseudo observations, vine fitting
R/backtest.R            rolling Value at Risk and coverage statistics
R/plots.R               vine structure and Value at Risk charts
legacy/original-analysis.R   the 2022 script, kept for reference
```

## Running it

```r
install.packages(c("readxl", "zoo", "e1071", "tseries", "rugarch", "VineCopula"))
```

Put the workbook at `data/Data Sheet 06-10-2020.xlsx`, or point `GVC_DATA_PATH` somewhere else,
then:

```bash
Rscript run_analysis.R output
```

Tables land in `output/` as csv and charts as png. Expect the rolling backtests to take a while,
they refit the model every ten observations across two full passes over twelve markets.

The workbook is not in this repository. It expects one `Date` column and one column of returns
per market, named exactly as in `MARKETS`.

## What changed from the original script

The original was a single 1,266 line file that ran top to bottom. The logic was sound, but each
market and each confidence level was a copy of the block above it, so the same twelve or thirty
six edits were needed for any change.

| Before | After |
|---|---|
| 12 near identical model fitting blocks | `fit_all_markets()` over `MARKETS` |
| 36 near identical backtest blocks, 12 markets by 3 levels | `backtest_all()` over markets and alphas |
| 24 near identical plotting blocks | `plot_all_backtests()` |
| Hard coded window sizes and market names throughout | `R/config.R` |
| Results read off the console | csv and png written to `output/` |
| Failure aborts the run | fits and rolls degrade to `NULL` with a warning |
| 14 packages loaded, several unused | 6, and no pipes, so the rest is base R |

Adding a market or a confidence level is now a one line change to the configuration.

The duplication was also hiding a bug. In the original, the Bangladesh block fitted
`bangladesh_fit` and then printed `usa_fit`, a copy paste slip that is invisible in a wall of
near identical code and impossible once the loop is written once.

## A note on model selection

The original chose its marginal by preferring the BIC **nearest zero**. For the variance models
it reported sGARCH -6.43, EGARCH -6.34 and GJR-GARCH -6.46, and selected EGARCH. For the
distributions it reported values from -6.34 for the normal to -6.658 for the skewed generalised
error, and again selected the normal.

Lower BIC is better regardless of sign, so on that convention GJR-GARCH would win the first
comparison and the skewed generalised error the second.

`rank_by_bic()` therefore orders ascending, lowest first. The configured marginal is still
AR(1) EGARCH(1,1) with normal innovations so the original results stay reproducible, but the
selection tables now show the ranking, and changing `MARGINAL` in `R/config.R` is all it takes
to follow it.

## Tests

```bash
Rscript tests/test-functions.R
```

16 checks covering interpolation, BIC ranking, date slicing, correlations, and the
configuration. They use base R only, so they run without rugarch or VineCopula installed and
make a fast check before committing to a full run.

## Status

Verified on R 4.6.1: every module parses, all 31 functions load, and the data preparation, BIC
ranking, date slicing and correlation helpers pass the checks above.

**Not yet reproduced end to end.** The workbook is not in this repository, so the fitting,
copula and backtesting paths have not been executed against real data. Anyone with the
spreadsheet should treat the first run as a replication check, and the numbers in the original
paper as the reference to match.

## License

See the repository for licensing. Original analysis 2022.
