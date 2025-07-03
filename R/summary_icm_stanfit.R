#' Summarize ICM Stanfit Object
#'
#' This function provides a summary method for objects of class `icm_stanfit`.
#' via a wrapper around the [extract_consensus()] function.
#'
#' @param object An object of class `icm_stanfit`.
#' @param ... Additional arguments (currently not used).
#'
#' @return A table with posterior medians and credible intervals for the
#' consensus intervals.
#'
#' @examples
#' \donttest{
#' # Create minimal example data
#' df_simplex <- data.frame(
#'   x1 = c(0.3, 0.4, 0.2, 0.5),
#'   x2 = c(0.3, 0.2, 0.4, 0.2),
#'   x3 = c(0.4, 0.4, 0.4, 0.3)
#' )
#' id_person <- c(1, 1, 2, 2)
#' id_item <- c(1, 2, 1, 2)
#'
#' # Fit ICM model
#' fit <- fit_icm(df_simplex, id_person, id_item, n_chains = 1,
#'                iter_sampling = 100, iter_warmup = 100,
#'                refresh = 0)
#'
#' # Get summary
#' summary(fit)
#' }
#'
#' @seealso [extract_consensus()]
#'
#' @exportS3Method intervalpsych::summary
summary.icm_stanfit <- function(object, ...) {
  list <- extract_consensus(object, print_summary = FALSE)

  return(list$summary)
}
