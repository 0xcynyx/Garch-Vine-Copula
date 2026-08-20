# Package loading with an actionable error when something is missing.

# The rewrite is base R apart from these six, down from fourteen in the original script.
REQUIRED_PACKAGES <- c("readxl", "zoo", "e1071", "tseries", "rugarch", "VineCopula")

# Load every required package, reporting all missing ones at once rather than the first.
load_packages <- function(packages = REQUIRED_PACKAGES) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing packages: ", paste(missing, collapse = ", "), "\n",
      "Install them with: install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "), "))",
      call. = FALSE
    )
  }
  invisible(lapply(packages, library, character.only = TRUE))
}
