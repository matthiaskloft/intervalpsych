#' The 'intervalpsych' package.
#'
#' @description The intervalpsych package provides tools for analyzing interval responses 
#' in psychometrics using the Interval Consensus Model (ICM). It implements Bayesian 
#' estimation via Stan, along with transformation and visualization functions for 
#' interval-valued data.
#' 
#' @details
#' The package provides the following main functionality:
#' 
#' \strong{Model Fitting:}
#' \itemize{
#'   \item \code{\link{fit_icm}()}: Fit the Interval Consensus Model using Stan
#'   \item \code{\link{extract_consensus}()}: Extract consensus intervals from fitted models
#' }
#' 
#' \strong{Data Transformations:}
#' \itemize{
#'   \item \code{\link{itvl_to_splx}()}/\code{\link{splx_to_itvl}()}: Convert between interval and simplex representations
#'   \item \code{\link{ilr}()}/\code{\link{inv_ilr}()}: Isometric log-ratio transformations
#'   \item \code{\link{slr}()}/\code{\link{inv_slr}()}: Symmetric log-ratio transformations
#'   \item \code{\link{remove_zeros}()}: Handle zero components in simplex data
#' }
#' 
#' \strong{Visualization:}
#' \itemize{
#'   \item \code{\link{plot_intervals}()}: Plot interval responses
#'   \item \code{\link{plot_intervals_cumulative}()}: Plot cumulative interval data
#'   \item \code{\link{plot_consensus}()}: Visualize consensus intervals
#'   \item \code{\link{theme_icm}()}: Custom ggplot2 theme for interval plots
#' }
#' 
#' \strong{Example Data:}
#' \itemize{
#'   \item \code{\link{quantifiers}}: Example dataset of quantifier interval responses
#' }
#' 
#' The Interval Consensus Model treats interval responses as arising from an underlying 
#' consensus process, where individual responses represent uncertainty around a latent 
#' consensus interval for each item. The model uses isometric log-ratio (ILR) transformations 
#' to map interval data to the real line for Bayesian estimation via Stan.
#'
#' @docType _PACKAGE
#' @name intervalpsych-package
#' @aliases intervalpsych
#' @useDynLib intervalpsych, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#'
#' @noRd
#'
#' @references
#' Stan Development Team (NA). RStan: the R interface to Stan. R package version 2.35.0.9000. https://mc-stan.org
#'
NULL
