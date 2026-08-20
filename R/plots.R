#' Charts for the vine structure and the value at risk backtests.

#' Plot realised returns against the value at risk forecasts for one market.
#'
#' @param rolls Named list of ugarchroll objects keyed by alpha.
#' @param dates Dates covering the backtest window.
#' @param market Market name, used as the y axis label.
#' @param colours Named colour vector.
plot_var_backtest <- function(rolls, dates, market, colours = VAR_COLOURS) {
  available <- rolls[!vapply(rolls, is.null, logical(1))]
  if (length(available) == 0) {
    warning("No rolling results to plot for ", market, call. = FALSE)
    return(invisible(NULL))
  }
  forecasts <- available[[1]]@forecast$VaR
  realised <- forecasts[, ncol(forecasts)]
  span <- seq_len(min(length(dates), length(realised)))
  plot(dates[span], realised[span], type = "l", col = colours[["actual"]], xlab = "Day", ylab = market)
  for (key in names(available)) {
    series <- available[[key]]@forecast$VaR[, 1]
    graphics::lines(dates[span], series[span], col = colours[[key]])
  }
  graphics::legend(
    "topright",
    legend = c("Daily return", sprintf("%s%% VaR", 100 * (1 - as.numeric(names(available))))),
    col = c(colours[["actual"]], unlist(colours[names(available)])),
    pch = 19
  )
  invisible(NULL)
}

#' Plot every market's backtest, optionally writing one file per market.
#'
#' @param results Output of backtest_all.
#' @param dates Dates covering the backtest window.
#' @param output_dir Directory for png files, or NULL to draw to the active device.
plot_all_backtests <- function(results, dates, output_dir = NULL) {
  if (!is.null(output_dir)) dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  for (market in names(results)) {
    if (!is.null(output_dir)) {
      grDevices::png(file.path(output_dir, paste0(tolower(market), "-var.png")), width = 1000, height = 600)
    }
    plot_var_backtest(results[[market]], dates, market)
    if (!is.null(output_dir)) grDevices::dev.off()
  }
  invisible(NULL)
}

#' Plot a fitted vine's dependence structure.
#'
#' @param vine A fitted RVineMatrix object.
#' @param output_path Optional png path.
plot_vine_structure <- function(vine, output_path = NULL) {
  if (!is.null(output_path)) {
    dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
    grDevices::png(output_path, width = 1000, height = 800)
  }
  plot(vine)
  if (!is.null(output_path)) grDevices::dev.off()
  invisible(NULL)
}
