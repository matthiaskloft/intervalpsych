#' Fit Item Response Theory Model
#'
#' This function fits an Item Response Theory (IRT) model using Stan.
#'
#' @param df_simplex A dataframe containing the simplex data.
#' @param id_person A vector of person indices.
#' @param id_item A vector of item indices.
#' @param item_labels A vector of item labels. Can be long format matching id_item or
#' a vector of unique labels in ascending order. Default is NULL.
#' @param link A character string specifying the link function. The only option as of now is "ilr", the Isometric Log-Ratio function.
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
           link = "ilr",
           iter_sampling = 500,
           iter_warmup = 500,
           n_chains = 4,
           n_cores = 1,
           adapt_delta = 0.9,
           ...) {

    ### Data Checks ------------------------------------------------------------

    # check that a valid link was specified
    link_functions <- c("ilr")
    if (!link %in% link_functions) {
      stop("Error: link must be either 'ilr' or 'clr'!")
    }

    # check if simplex is a dataframe
    if (is.data.frame(df_simplex) == FALSE) {
      stop("Error: simplex must be a dataframe!")
    }

    # check length of person indices
    if (length(id_person) != nrow(df_simplex)) {
      stop("Error: id_person must have the same length as the number of rows in the simplex!")
    }

    # check length of person indices
    if (length(id_item) != nrow(df_simplex)) {
      stop("Error: id_item must have the same length as the number of rows in the simplex!")
    }

    # if item_labels is not NULL, check that length of item_labels is either equal to the number of rows in the simplex or equal to the unique elements in id_item
    if (!is.null(item_labels)) {
      if (length(item_labels) != nrow(df_simplex) & length(item_labels) != length(unique(id_item))) {
        stop("Error: item_labels must have the same length as the number of rows in the simplex or the number of unique elements in id_item!")
      }
    }

    # check that person and item IDs are natural numbers
    if (all(id_person %% 1 == 0) == FALSE) {
      stop("Error: id_person must be natural numbers!")
    }

    if (all(id_item %% 1 == 0) == FALSE) {
      stop("Error: id_item must be natural numbers!")
    }


    # check for NAs
    if (any(is.na(df_simplex))) {
      stop("Error: simplex contains NAs!")
    }

    # get number of cols
    n_elements <- ncol(df_simplex)

    if (n_elements != 3) {
      stop("Simplex must have 3 elements")
    }

    # run log-ratio checks
    for (i in 1:nrow(df_simplex)) {
      check_simplex(as.matrix(df_simplex)[i, ])
    }




    ### Recompute indices and labels -------------------------------------------
    id_person <- as.numeric(factor(id_person))
    id_item <- as.numeric(factor(id_item))
    if(!is.null(item_labels)) {
      if(length(item_labels) == nrow(df_simplex)) {
        item_labels <- unique(item_labels)
      }
    } else {
      item_labels <- 1:max(id_item)
    }

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
