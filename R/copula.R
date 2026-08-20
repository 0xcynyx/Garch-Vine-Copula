#' Standardised residuals, pseudo observations, and the vine copula fit.

#' Standardised residuals for every fitted market, aligned in one matrix.
#'
#' @param fits Named list of ugarchfit objects.
#' @return A numeric matrix with one column per market.
standardised_residuals <- function(fits) {
  usable <- fits[!vapply(fits, is.null, logical(1))]
  if (length(usable) == 0) stop("No successful fits to extract residuals from.", call. = FALSE)
  columns <- lapply(usable, function(fit) as.numeric(rugarch::residuals(fit, standardize = TRUE)))
  matrix_out <- do.call(cbind, columns)
  colnames(matrix_out) <- names(usable)
  matrix_out
}

#' Convert residuals to uniform margins, which is what a copula requires.
#'
#' @param residual_matrix Matrix of standardised residuals.
to_pseudo_observations <- function(residual_matrix) {
  apply(residual_matrix, MARGIN = 2, VineCopula::pobs)
}

#' Select and fit a vine copula structure.
#'
#' @param pseudo_observations Matrix with uniform margins.
#' @param type Vine family, RVine or CVine.
#' @param selection_criterion Criterion used for pair copula selection.
#' @param tree_criterion Criterion used to build the trees.
#' @param progress Whether to print fitting progress.
fit_vine <- function(pseudo_observations,
                     type = VINE$type,
                     selection_criterion = VINE$selection_criterion,
                     tree_criterion = VINE$tree_criterion,
                     progress = TRUE) {
  VineCopula::RVineStructureSelect(
    data = pseudo_observations,
    progress = progress,
    type = type,
    selectioncrit = selection_criterion,
    treecrit = tree_criterion
  )
}

#' Structure summary and BIC for a fitted vine.
#'
#' @param vine A fitted RVineMatrix object.
#' @return A list holding the structure summary and the BIC.
describe_vine <- function(vine) {
  list(structure = summary(vine), bic = vine$BIC)
}

#' Fit both vine families reported in the study.
#'
#' @param pseudo_observations Matrix with uniform margins.
#' @return A named list of fitted vines.
fit_vine_families <- function(pseudo_observations) {
  list(
    rvine = fit_vine(pseudo_observations, type = "RVine"),
    cvine = fit_vine(pseudo_observations, type = "CVine")
  )
}
