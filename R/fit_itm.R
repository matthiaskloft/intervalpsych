#' Fit Item Response Theory Model
#'
#' This function fits an Item Response Theory (IRT) model using Stan.
#'
#' @param df_simplex A dataframe containing the simplex data.
#' @param id_person A vector of person indices.
#' @param id_item A vector of item indices.
#' @param item_labels A vector of item labels. Can be long format matching id_item or
#' a vector of unique labels in ascending order. Default is NULL.
#' @param iter_sampling An integer specifying the number of sampling iterations. Default is 500.
#' @param iter_warmup An integer specifying the number of warmup iterations. Default is 500.
#' @param n_chains An integer specifying the number of Markov chains. Default is 4.
#' @param n_cores An integer specifying the number of cores to use. Default is 1.
#' @param adapt_delta A numeric value specifying the target acceptance rate. Default is 0.9.
#' @param ... Additional arguments passed to the \code{\link[rstan:sampling]{rstan::sampling}} function.
#'
#' @return A fitted Stan model object.
#' @export
#'
#' @examples
#' \dontrun{
#' df_simplex <- data.frame(matrix(runif(100), nrow=10))
#' id_person <- rep(1:5, each=2)
#' id_item <- rep(1:2, times=5)
#' fit <- fit_itm(df_simplex, id_person, id_item)
#' }
fit_itm <-
  function(df_simplex,
           id_person,
           id_item,
           item_labels = NULL,
           iter_sampling = 500,
           iter_warmup = 500,
           n_chains = 4,
           n_cores = 1,
           adapt_delta = 0.9,
           ...) {

    ### Data Checks ------------------------------------------------------------

    # check if simplex is a dataframe
    if (is.data.frame(df_simplex) == FALSE) {
      stop("Error: simplex must be a dataframe!")
    }

    # check indices



    # check for NAs





    ### Stan Data --------------------------------------------------------------

    stan_data <- list(
      I = max(id_person),
      J = max(id_item),
      N = nrow(df_simplex),
      ii = id_person,
      jj = id_item,
      nn = 1:nrow(df_simplex),
      Y_splx = df_simplex
    )

    ### Run Sampler Stan Model -------------------------------------------------

    # specify default arguments
    default_args <- list(
      object = stanmodels$itm,
      data = stan_data,
      pars = c("Tr_loc_beta",
               "Tr_wid_beta",
               "I_raw",
               "L_corr_E",
               "J_raw",
               "L_corr_lambda"),
      include = FALSE,
      chains = n_chains,
      cores = n_cores,
      iter = iter_sampling + iter_warmup,
      warmup = iter_warmup,
      verbose = FALSE,
      control = list(adapt_delta = adapt_delta)
    )

    # Run sampler
    stan_fit <- do.call(rstan::sampling,
                        utils::modifyList(default_args, list(...)))


    ### Return Object ----------------------------------------------------------

    ret_fit <- list(
      stan_fit = stan_fit,
      stan_data = stan_data,
      item_labels = item_labels
    )

    class(ret_fit) <- c("itm_stanfit", class(ret_fit))

    return(ret_fit)

  }
