#' The 'intervalpsych' package.
#'
#' @description The intervalpsych package provides tools for analyzing
#' continuous bounded interval responses in psychometrics. It implements the
#' Interval Consensus Model (ICM, Kloft et al., 2024), which is a consensus model
#' for such data.
#'
#' @name intervalpsych-package
#' @aliases intervalpsych
#' @useDynLib intervalpsych, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#' @keywords internal
#' @references
#' Kloft, M., Siepe, B. S., & Heck, D. W. (2024).
#' The Interval Truth Model: A Consensus Model for Continuous Bounded Interval Responses.
#' \doi{doi:10.31234/osf.io/dzvw2}
#'
"_PACKAGE"
