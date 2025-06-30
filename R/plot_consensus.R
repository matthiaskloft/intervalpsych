#' Plot ICM Consensus Intervals
#'
#' This function generates plots for ICM Stanfit results using either the median bounds method or the draws gradient method.
#'
#' @param icm_stanfit An object of class 'icm_stanfit' containing the Stanfit results.
#' @param method A character string specifying the plotting method. Options are "median_bounds" (default) or "draws_distribution".
#' @param CI A numeric value specifying the confidence interval for the "draws_distribution" method. Default is 0.95.
#' This can also be a vector of length 2 for multiple confidence intervals.
#'
#' @return A ggplot2 object representing the interval plot.
#'
#' @details
#' The function checks if the input object is of class 'icm_stanfit'. It then extracts the draws for the lower and upper bounds (T_L and T_U) from the Stanfit object.
#'
#' If the method is "median_bounds", the function calculates the median of the lower and upper bounds and generates an interval plot using these medians.
#'
#' If the method is "draws_distribution", the function checks the number of draws and warns if it is less than 1000. It then computes a consensus distribution for each item and generates a gradient interval plot.
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
#' # Plot consensus intervals using median bounds
#' plot_consensus(fit, method = "median_bounds")
#' }
#'
#' @export
#'
plot_consensus <- function(icm_stanfit,
                           method = "median_bounds",
                           CI = .95) {
  # check: is class "icm_stanfit"?
  if (!inherits(icm_stanfit, "icm_stanfit")) {
    stop("Input must be an object of class 'icm_stanfit'")
  }

  # get draws
  T_L <- rstan::extract(icm_stanfit$stan_fit, pars = "Tr_L")[[1]] |>  posterior::rvar()
  names(T_L) <- paste0("T_L_", 1:icm_stanfit$stan_fit@par_dims$Tr_L)

  T_U <- rstan::extract(icm_stanfit$stan_fit, pars = "Tr_U")[[1]] |>  posterior::rvar()
  names(T_U) <- paste0("T_U_", 1:icm_stanfit$stan_fit@par_dims$Tr_U)

  # create a data.frame with rvars
  df_rvar <- data.frame(item = icm_stanfit$item_labels,
                        T_L = T_L,
                        T_U = T_U)

  ### method: median bounds ---------------------------------------------------

  if (method == "median_bounds") {
    # get summary
    summary <- df_rvar |>
      dplyr::reframe(T_L_median = stats::median(T_L),
                     T_U_median = stats::median(T_U))
    # plot
    interval_plot <-
      plot_intervals(df_interval_bounds = summary[, c("T_L_median", "T_U_median")],
                     item_labels = icm_stanfit$item_labels)

    return(interval_plot)
  }


  ### method: draws_gradient ---------------------------------------------------

  if (method == "draws_distribution") {
    # check number of draws
    n_iter <- attributes(df_rvar$T_L[1])$draws |> length()

    # warn if number of draws < 1000
    if (n_iter < 2000) {
      warning(
        "Number of draws is less than 1000. Consider increasing the number of iterations or using method = 'median_bounds' instead."
      )
    }


    # prepare runif for rvar format
    rvar_unif <- posterior::rfun(stats::runif)
    # compute consensus distribution for each item
    df_rvar$consensus <- rvar_unif(n = nrow(df_rvar),
                                   min = df_rvar$T_L,
                                   max = df_rvar$T_U)

    # plot
    interval_plot <-
      df_rvar |>
      ggplot2::ggplot() +
      ggdist::stat_halfeye(ggplot2::aes(xdist = .data$consensus, y = .data$item),
                           .width = CI) +
      ggplot2::scale_x_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, .25),
        labels = c("0", ".25", ".50", ".75", "1"),
        expand = ggplot2::expansion(0, 0)
      ) +
      ggplot2::labs(x = "Interval Response", y = "Item") +
      theme_icm(base_size = 12, hide_axis_text_y = FALSE)

    return(interval_plot)
  }

}
