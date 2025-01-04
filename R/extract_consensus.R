#' Extract Consensus intervals from ITM Stan Fit Object
#'
#' This function extracts parameter estimates for the consensus intervals from a
#' fitted Interval Truth Model Stan fit object of class `itm_stanfit`.
#'
#' @param itm_stanfit An object of class `itm_stanfit` containing the fitted Stan model.
#' @param print_summary A logical value indicating whether to print a summary of the extracted parameters. Default is `TRUE`.
#'
#' @return A list containing:
#' \item{df_rvar}{A data frame with extracted posterior samples as random variables.}
#' \item{summary}{A summary data frame with median and credible intervals for the extracted parameters.}
#' \item{item_labels}{A character vector of item labels.}
#'
#' @details
#' This function extracts parameter estimates for the consensus intervals from a
#' fitted Interval Truth Model Stan fit object of class `itm_stanfit`.
#'
#'
#' @importFrom rstan extract
#' @importFrom posterior rvar
#' @importFrom dplyr reframe
#' @export
extract_consensus <-
  function(itm_stanfit,
           print_summary = TRUE
           ) {
    # check: is class "itm_stanfit"?
    if (!inherits(itm_stanfit, "itm_stanfit")) {
      stop("Input must be an object of class 'itm_stanfit'")
    }

    # extract posterior samples
    T_loc <- rstan::extract(itm_stanfit$stan_fit, pars = "Tr_loc_splx")[[1]] |>  posterior::rvar()
    names(T_loc) <- paste0("T_loc_", 1:itm_stanfit$stan_fit@par_dims$Tr_loc_splx)

    T_wid <- rstan::extract(itm_stanfit$stan_fit, pars = "Tr_wid_splx")[[1]] |> posterior::rvar()
    names(T_wid) <- paste0("T_wid_", 1:itm_stanfit$stan_fit@par_dims$Tr_wid_splx)

    T_L <- rstan::extract(itm_stanfit$stan_fit, pars = "Tr_L")[[1]] |>  posterior::rvar()
    names(T_L) <- paste0("T_L_", 1:itm_stanfit$stan_fit@par_dims$Tr_L)

    T_U <- rstan::extract(itm_stanfit$stan_fit, pars = "Tr_U")[[1]] |>  posterior::rvar()
    names(T_U) <- paste0("T_U_", 1:itm_stanfit$stan_fit@par_dims$Tr_U)

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
    if (!is.null(itm_stanfit$item_labels)) {
      rownames(summary) <- itm_stanfit$item_labels
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



#' @exportS3Method intervalpsych::summary
#' @noRd
summary.itm_stanfit <- function(object, ...) {

  list <- extract_consensus(object, print_summary = FALSE)

  return(list$summary)
}
