#' Summarize Stanfit Object
#'
#' This function provides a summary for an object of class `icm_stanfit`.
#'
#' @param object An object of class `icm_stanfit`.
#' @param ... Additional arguments (currently not used).
#'
#' @return A summary of the `icm_stanfit` object.
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
#' fit <- fit_icm(df_simplex, id_person, id_item, 
#'                iter_sampling = 100, iter_warmup = 100)
#' 
#' # Get summary
#' summary(fit)
#' }
#'
#' @exportS3Method intervalpsych::summary
summary.icm_stanfit <- function(object, ...) {

  list <- extract_consensus(object, print_summary = FALSE)

  return(list$summary)
}
