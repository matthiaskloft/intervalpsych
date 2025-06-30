#' Fit Interval Consensus Model
#'
#' This function fits the Interval Consensus Model (ICM, Kloft et al., 2024) using Stan.
#'
#' @param df_simplex A dataframe containing the simplex data.
#' @param id_person A vector of person indices.
#' @param id_item A vector of item indices.
#' @param item_labels A vector of item labels. Can be long format matching id_item or
#' a vector of unique labels in ascending order. Default is NULL.
#' @param link A character string specifying the link function. Options are "ilr" (Isometric Log-Ratio) or "slr" (Sum Log-Ratio).
#' See also [ilr()] and [slr()] for details. Default is "ilr".
#' @param padding Padding constant that was used to remove zero-components from the simplex. Default is 0.
#' The model will reverse the padding when transforming results back to the original interval response scale. See also [remove_zeros()] for details.
#' @param iter_sampling An integer specifying the number of sampling iterations. Default is 500.
#' @param iter_warmup An integer specifying the number of warmup iterations. Default is 500.
#' @param n_chains An integer specifying the number of Markov chains. Default is 4.
#' @param n_cores An integer specifying the number of cores to use. Default is 1.
#' @param adapt_delta A numeric value specifying the target acceptance rate. Default is 0.9.
#' @param ... Additional arguments passed to the [rstan::sampling()] function.
#'
#' @return A fitted Stan model object of class `icm_stanfit` containing the following components:
#' \describe{
#'   \item{stan_model}{The compiled Stan model object}
#'   \item{stan_fit}{The fitted Stan model with posterior samples for the following parameters:}
#'   \item{stan_data}{The data list passed to Stan}
#'   \item{item_labels}{Vector of item labels}
#' }
#'
#' The `stan_fit` component contains posterior samples for these ICM parameters:
#' \describe{
#'   \item{Person Parameters:}{
#'     \itemize{
#'       \item `E_loc` - Person competence for location (I×1 vector)
#'       \item `E_wid` - Person competence for width (I×1 vector)
#'       \item `a_loc` - Person scaling bias for location (I×1 vector)
#'       \item `b_loc` - Person shifting bias for location (I×1 vector)
#'       \item `b_wid` - Person shifting bias for width (I×1 vector)
#'       \item `rho_E` - Correlation between person competences for location and width
#'     }
#'   }
#'   \item{Item Parameters:}{
#'     \itemize{
#'       \item `Tr_loc` - Item consensus location in transformed space (J×1 vector)
#'       \item `Tr_wid` - Item consensus width in transformed space (J×1 vector)
#'       \item `Tr_loc_splx` - Item consensus location in simplex space (J×1 vector)
#'       \item `Tr_wid_splx` - Item consensus width in simplex space (J×1 vector)
#'       \item `Tr_L` - Item consensus lower bound (J×1 vector)
#'       \item `Tr_U` - Item consensus upper bound (J×1 vector)
#'       \item `Tr_splx` - Item consensus simplex representation (J×3 matrix)
#'       \item `lambda_loc` - Item difficulty/discernibility for location (J×1 vector)
#'       \item `lambda_wid` - Item difficulty/discernibility for width (J×1 vector)
#'       \item `omega` - Item residual correlations between location and width (J×1 vector)
#'       \item `rho_lambda` - Correlation between item difficulties for location and width
#'     }
#'   }
#'   \item{Hyperparameters:}{
#'     \itemize{
#'       \item `mu_E` - Hyperprior means for person competences (2×1 vector)
#'       \item `sigma_I` - Hyperprior scales for person parameters (5×1 vector)
#'       \item `sigma_lambda` - Hyperprior scales for item difficulties (2×1 vector)
#'     }
#'   }
#'   \item{Posterior Predictive Checks:}{
#'     \itemize{
#'       \item `Y_ppc_loc` - Predicted responses for location (N×1 vector)
#'       \item `Y_ppc_wid` - Predicted responses for width (N×1 vector)
#'       \item `Y_ppc_splx` - Predicted responses in simplex space (N×3 matrix)
#'       \item `Y_ppc_loc_splx` - Predicted location responses in simplex space (N×1 vector)
#'       \item `Y_ppc_wid_splx` - Predicted width responses in simplex space (N×1 vector)
#'     }
#'   }
#' }
#'
#' Where I = number of persons, J = number of items, N = number of observations.
#' @export
#' @references
#' Kloft, M., Siepe, B. S., & Heck, D. W. (2024).
#' The Interval Truth Model: A Consensus Model for Continuous Bounded Interval Responses.
#' \doi{doi:10.31234/osf.io/dzvw2}
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
#' # Fit ICM model (reduce iterations for faster example)
#' fit <- fit_icm(df_simplex, id_person, id_item, chains = 1,
#'                iter_sampling = 100, iter_warmup = 100)
#' }
fit_icm <-
  function(df_simplex,
           id_person,
           id_item,
           item_labels = NULL,
           link = "ilr",
           padding = 0,
           iter_sampling = 500,
           iter_warmup = 500,
           n_chains = 4,
           n_cores = 1,
           adapt_delta = 0.9,
           ...) {

    ### Data Checks ------------------------------------------------------------

    # check that a valid link was specified
    link_functions <- c("ilr", "slr")
    if (!link %in% link_functions) {
      stop("Error: link must be either 'ilr' or 'slr'!")
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

    # check padding constant is a real number between 0 and 1
    if (!is.numeric(padding) || length(padding) != 1 || padding < 0 || padding > 1) {
      stop("Error: padding must be a real number between 0 and 1!")
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
      Y_splx = df_simplex,
      padding = padding
    )

    ### Stan Model --------------------------------------------------------------

    if (link == "ilr") {
      stan_model <- stanmodels$icm_ilr
    } else if (link == "slr") {
      stan_model <- stanmodels$icm_slr
    }

    ### Run Sampler Stan Model -------------------------------------------------



    # specify default arguments
    default_args <- list(
      object = stan_model,
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
      stan_model = stan_model,
      stan_fit = stan_fit,
      stan_data = stan_data,
      item_labels = item_labels
    )

    class(ret_fit) <- c("icm_stanfit", class(ret_fit))

    return(ret_fit)

  }
