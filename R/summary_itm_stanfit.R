#' Summarize Stanfit Object
#'
#' This function provides a summary for an object of class `itm_stanfit`.
#'
#' @param object An object of class `itm_stanfit`.
#' @param ... Additional arguments (currently not used).
#'
#' @return A summary of the `itm_stanfit` object.
#'
#' @examples
#' \dontrun{
#'   # Assuming `fit` is an object of class `itm_stanfit`
#'   summary(fit)
#' }
#'
#' @exportS3Method intervalpsych::summary
#' @noRd
summary.itm_stanfit <- function(object, ...) {

  list <- extract_consensus(object, print_summary = FALSE)

  return(list$summary)
}
