#' Extract Consensus intervals from ICM Stan Fit Object
#'
#' This function extracts parameter estimates for the consensus intervals from a
#' fitted Interval Consensus Model Stan fit object of class `icm_stanfit`.
#'
#' @param icm_stanfit An object of class `icm_stanfit` containing the fitted Stan model.
#' @param print_summary A logical value indicating whether to print a summary of the extracted parameters. Default is `TRUE`.
#'
#' @return A list containing:
#' \item{df_rvar}{A data frame with extracted posterior samples as random variables.}
#' \item{summary}{A summary data frame with median and credible intervals for the extracted parameters.}
#'
#' @details
#' This function extracts parameter estimates for the consensus intervals from a
#' fitted Interval Consensus Model Stan fit object of class `icm_stanfit`.
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
#' # Extract consensus intervals
#' consensus <- extract_consensus(fit)
#' }
#'
#' @importFrom rstan extract
#' @importFrom posterior rvar
#' @importFrom dplyr reframe
#' @export
extract_consensus <-
  function(icm_stanfit,
           print_summary = TRUE
           ) {
    # check: is class "icm_stanfit"?
    if (!inherits(icm_stanfit, "icm_stanfit")) {
      stop("Input must be an object of class 'icm_stanfit'")
    }

    # extract posterior samples
    T_loc <- rstan::extract(icm_stanfit$stan_fit, pars = "Tr_loc_splx")[[1]] |>  posterior::rvar()
    names(T_loc) <- paste0("T_loc_", 1:icm_stanfit$stan_fit@par_dims$Tr_loc_splx)

    T_wid <- rstan::extract(icm_stanfit$stan_fit, pars = "Tr_wid_splx")[[1]] |> posterior::rvar()
    names(T_wid) <- paste0("T_wid_", 1:icm_stanfit$stan_fit@par_dims$Tr_wid_splx)

    T_L <- rstan::extract(icm_stanfit$stan_fit, pars = "Tr_L")[[1]] |>  posterior::rvar()
    names(T_L) <- paste0("T_L_", 1:icm_stanfit$stan_fit@par_dims$Tr_L)

    T_U <- rstan::extract(icm_stanfit$stan_fit, pars = "Tr_U")[[1]] |>  posterior::rvar()
    names(T_U) <- paste0("T_U_", 1:icm_stanfit$stan_fit@par_dims$Tr_U)

    # create a data.frame with rvars
    df_rvar <- data.frame(
      T_loc = T_loc,
      T_wid = T_wid,
      T_L = T_L,
      T_U = T_U
    )

    # compute short summary
    summary <- df_rvar |>
      dplyr::reframe(
        T_L_median = stats::median(T_L),
        T_L_CI_025 = t(stats::quantile(T_L, 0.025)),
        T_L_CI_975 = t(stats::quantile(T_L, 0.975)),
        T_U_median = stats::median(T_U),
        T_U_CI_025 = t(stats::quantile(T_U, 0.025)),
        T_U_CI_975 = t(stats::quantile(T_U, 0.975))
      )

    # append labels
    if (!is.null(icm_stanfit$item_labels)) {
      rownames(summary) <- icm_stanfit$item_labels
    }

    # print summary
    if (print_summary) {
      print(summary |> round(2))
    }

    # output
    ret_out <- list(df_rvar = df_rvar,
                    summary = summary)

    return(ret_out)

  }



