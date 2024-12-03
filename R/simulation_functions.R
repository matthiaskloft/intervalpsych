#' Simulate interval response data from the Interval Truth Model (ITM; Kloft et al., 2024)
#'
#' @param n_respondents Number of respondents.
#' @param n_items Number of Items.
#' @param Tr_loc aa
#' @param Tr_wid aa
#' @param E_loc aa
#' @param E_wid aa
#' @param lambda_loc aa
#' @param lambda_wid aa
#' @param a_loc aa
#' @param b_loc aa
#' @param b_wid aa
#'
#'
#' @description
#' Simulate data from the Interval Truth Model (ITM; Kloft et al., 2024).
#'
#'
#'
#' @return A dataframe of indices, simulated responses, and data generating parameters.
#' + ii: person indices
#' + jj: item indices
#' + x1: leftmost component of the response simplex.
#' + x2: middle component of the response simplex.
#' + x3: rightmost component of the response simplex.
#' + intvl_loc: interval location.
#' + intvl_wid: interval width.
#' + generating person parameters:
#'    + E_loc:
#'    + E_wid:
#'    + a_loc:
#'    + b_loc:
#'    + b_wid:
#' + generating item parameters:
#'    + Tr_loc:
#'    + Tr_wid:
#'    + lambda_loc:
#'    + lambda_wid:
#'
#'
#' @export
#'
#' @examples
#' sim_itm(n_respondents = 10, n_items = 5)
#'
#'
sim_itm <-  function(n_respondents,
                     n_items,
                     link = "ilr",
                     # c("ilr", "sb")
                     mu_Tr_loc = NULL,
                     mu_Tr_wid = NULL,
                     mu_E_loc = NULL,
                     mu_E_wid = NULL,
                     sigma_Tr_loc = NULL,
                     sigma_Tr_wid = NULL,
                     sigma_lambda_E_loc = NULL,
                     sigma_lambda_E_wid = NULL,
                     sigma_a_loc = NULL,
                     sigma_b_loc = NULL,
                     sigma_b_wid = NULL,
                     omega_beta = NULL) {
  ### Hyper Priors ###

  # compute a benchmark for the mean and SD of the parameters
  if (link == "ilr") {
    mean_benchmark <- ilr(c(.4, .2, .4))
  } else{
    mean_benchmark <- ilr(c(.425, .15, .425))
  }
  sd_benchmark_loc <- ilr(c(.98, .01, .01))
  sd_benchmark_wid <- ilr(c(.495, .01, .495))

  # mean for Tr_loc
  mu_Tr_loc <- ifelse(is.null(mu_Tr_loc), mean_benchmark[1], mu_Tr_loc)
  # mean for Tr_wid
  mu_Tr_wid <- ifelse(is.null(mu_Tr_wid), mean_benchmark[2], mu_Tr_wid)
  # SD forTr_loc
  sigma_Tr_loc <- ifelse(is.null(sigma_Tr_loc), sd_benchmark_loc[1] / 4, sigma_Tr_loc)
  # SD Tr_wid
  sigma_Tr_wid <- ifelse(is.null(sigma_Tr_wid),
                         abs(sd_benchmark_wid[2] - mean_benchmark[2]) / 4,
                         sigma_Tr_wid)

  # mean fro E_loc
  mu_E_loc <- ifelse(is.null(mu_E_loc), log(sigma_Tr_loc) , mu_E_loc)
  # mean for E_wid
  mu_E_wid <- ifelse(is.null(mu_E_wid), log(sigma_Tr_wid), mu_E_wid)

  # SDs for other parameters
  sigma_lambda_E_loc <- ifelse(is.null(sigma_lambda_E_loc), .3, sigma_lambda_E_loc)
  sigma_lambda_E_wid <- ifelse(is.null(sigma_lambda_E_wid), .3, sigma_lambda_E_wid)
  sigma_a_loc <- ifelse(is.null(sigma_a_loc), .3, sigma_a_loc)
  sigma_b_loc <- ifelse(is.null(sigma_b_loc), sigma_Tr_loc / 3, sigma_b_loc)
  sigma_b_wid <- ifelse(is.null(sigma_b_wid), sigma_Tr_wid / 3, sigma_b_wid)
  if (is.null(omega_beta)) {
    omega_beta <- 3
  } else{
    omega_beta <- omega_beta
  }

  ### Indices ###
  n <- n_respondents * n_items
  ii <- rep(1:n_respondents, each = n_items)
  jj <- rep(1:n_items, times = n_respondents)

  ### True Parameters ###

  # true intervals
  Tr_loc <- stats::rnorm(n_items, mu_Tr_loc, sigma_Tr_loc)
  Tr_wid <- stats::rnorm(n_items, mu_Tr_wid, sigma_Tr_wid)
  if (link == "ilr") {
    Tr_splx <- cbind(Tr_loc, Tr_wid) |> inv_ilr()
  } else {
    Tr_splx <- cbind(Tr_loc, Tr_wid) |> inv_ilr()
  }
  Tr_L <- Tr_splx[, 1]
  Tr_U <- 1 - Tr_splx[, 3]

  # discernibility
  lambda_loc <- 1 / exp(rnorm(n_items, 0, sigma_lambda_E_loc))
  lambda_wid <- 1 / exp(rnorm(n_items, 0, sigma_lambda_E_wid))
  # respondent proficiency
  E_loc <- 1 / exp(rnorm(n_respondents, mu_E_loc, sigma_lambda_E_loc))
  E_wid <- 1 / exp(rnorm(n_respondents, mu_E_wid, sigma_lambda_E_wid))
  # respondent scaling bias
  a_loc <- exp(rnorm(n_respondents, 0, sigma_a_loc))
  # respondent shifting bias
  b_loc <- stats::rnorm(n_respondents, 0, sigma_b_loc)
  b_wid <- stats::rnorm(n_respondents, 0, sigma_b_wid)

  ### Model ###

  # expected interval
  mu_loc <- Tr_loc[jj] * a_loc[ii] + b_loc[ii]
  mu_wid <- Tr_wid[jj]             + b_wid[ii]
  # precision
  sigma_loc <- a_loc[ii] / E_loc[ii] / lambda_loc[jj]
  sigma_wid <- 1 / E_wid[ii] / lambda_wid[jj]
  # residual correlation
  omega <- stats::rbeta(n = n_items,
                        shape1 = omega_beta,
                        shape2 = omega_beta) * 2 - 1

  # generate unbounded interval response data
  Y <- extraDistr::rbvnorm(
    n = n,
    mean1 =  mu_loc,
    mean2 = mu_wid,
    sd1 = sigma_loc,
    sd2 = sigma_wid,
    cor = omega[jj]
  )
  Y_loc <- Y[, 1]
  Y_wid <- Y[, 2]

  # convert to bounded interval response data
  if (link == "ilr") {
    X <- inv_ilr(Y)
  } else {
    X <- inv_ilr(Y)
  }

  ### Save Objects ###

  # data frame of responses
  responses <- data.frame(
    ii = ii,
    jj = jj,
    Y_loc,
    Y_wid,
    x_splx_1 = X[, 1],
    x_splx_2 = X[, 2],
    x_splx_3 = X[, 3],
    sum = rowSums(X),
    x_L = X[, 1],
    x_U = 1 - X[, 3]
  )

  # list of true parameters
  parameters <- list(
    Tr_loc = Tr_loc,
    Tr_wid = Tr_wid,
    Tr_splx = Tr_splx,
    Tr_L = Tr_L,
    Tr_U = Tr_U,
    lambda_loc = lambda_loc,
    lambda_wid = lambda_wid,
    E_loc = E_loc,
    E_wid = E_wid,
    a_loc = a_loc,
    b_loc = b_loc,
    b_wid = b_wid,
    omega_beta = omega_beta,
    omega = omega,
    mu_Tr_loc = mu_Tr_loc,
    mu_Tr_wid = mu_Tr_wid,
    sigma_Tr_loc = sigma_Tr_loc,
    sigma_Tr_wid = sigma_Tr_wid,
    sigma_lambda_E_loc = sigma_lambda_E_loc,
    sigma_lambda_E_wid = sigma_lambda_E_wid,
    sigma_a_loc = sigma_a_loc,
    sigma_b_loc = sigma_b_loc,
    sigma_b_wid = sigma_b_wid,
    link = link
  )
  sim_data <- list(responses = responses, parameters = parameters)
  return(sim_data)
}


## Simulate interval response data from the Dirichlet Dual Response Model (DDRM)
#'
#' @title Simulate interval response data from the Dirichlet Dual Response Model (DDRM)
#'
#' @param n_respondents Number of respondents.
#' @param n_items Number of Items.
#' @param alpha_loc Item scaling parameters for the location dimension. If NULL, these will be simulated.
#' @param alpha_wid Item scaling parameters for the width / expansion dimension. If NULL, these will be simulated.
#' @param delta Item difficulty parameters. If NULL, these will be simulated.
#' @param gamma Item expansion parameters. If NULL, these will be simulated.
#' @param tau Item precision parameters. If NULL, these will be simulated.
#'
#' @description
#' Simulate data from the Dirichlet Dual Response Model (DDRM; Kloft et al., 2024)
#'
#' @return A dataframe of indices, simulated responses, and data generating parameters.
#' + ii: person indices
#' + jj: item indices
#' + x1: leftmost component of the response simplex.
#' + x2: middle component of the response simplex.
#' + x3: rightmost component of the response simplex.
#' + intvl_loc: interval location.
#' + intvl_wid: interval width.
#' + generating person parameters:
#'    + theta: person location.
#'    + eta: person width / expansion.
#' + generating item parameters:
#'    + alpha_loc: location scaling.
#'    + alpha_wid: width / expansion scaling.
#'    + delta: item difficulty.
#'    + gamma: item width / expansion.
#'    + tau: item precision.
#'
#' @export
#'
#' @examples
#' sim_ddrm(n_respondents = 10, n_items = 5)
#'
#'
sim_ddrm <- function(n_respondents = 50,
                     n_items = 5,
                     alpha_loc = NULL,
                     alpha_wid = NULL,
                     delta = NULL,
                     gamma = NULL,
                     tau = NULL) {
  # simulate missing item parameters
  if (is.null(alpha_loc)) {
    alpha_loc <- stats::runif(n_items, .3, .5)
  }
  if (is.null(alpha_wid)) {
    alpha_wid <- stats::runif(n_items, .3, .5)
  }
  if (is.null(delta)) {
    delta <- stats::rnorm(n_items)
  }
  if (is.null(gamma)) {
    gamma <- stats::rnorm(n_items)
  }
  if (is.null(tau)) {
    tau <- stats::runif(n_items, .1, 1)
  }

  item_params <- data.frame(jj = 1:n_items, alpha_loc, alpha_wid, delta, gamma, tau)
  # simulate person parameters
  theta <- stats::rnorm(n_respondents)
  eta <- stats::rnorm(n_respondents)

  person_params <- data.frame(ii = 1:n_respondents, theta, eta)
  # build indices
  ii <- rep(1:n_respondents, n_items)
  jj <- rep(1:n_items, each = n_respondents)

  # compute dirichlet parameters
  dir_params <- as.data.frame(matrix(NA, nrow = n_respondents * n_items, ncol = 3))

  for (n in 1:length(ii)) {
    dir_params[n, 1] <-
      exp(alpha_loc[jj[n]] * (theta[ii[n]] + delta[jj[n]]) + tau[jj[n]])
    dir_params[n, 2] <-
      exp(alpha_wid[jj[n]] * (eta[ii[n]] - gamma[jj[n]]) + tau[jj[n]])
    dir_params[n, 3] <-
      exp(-alpha_loc[jj[n]] * (theta[ii[n]] + delta[jj[n]]) + tau[jj[n]])
  }

  # simulate respnses
  responses <- as.data.frame(t(apply(
    X = dir_params, MARGIN = 1, FUN = rdirichlet
  )))

  names(responses) <- c("x1", "x2", "x3")
  # Compute interval location and width
  responses$intvl_loc <- responses$x1 + responses$x2 / 2
  responses$intvl_wid <- responses$x2 - responses$x1

  # join data
  suppressMessages({
    df_out <- cbind(ii, jj, responses)
    df_out <- dplyr::full_join(df_out, person_params)
    df_out <- dplyr::full_join(df_out, item_params)
  })
  return(df_out)
}



# helper: draw one sample from a dirichlet distribution---------------------------------
rdirichlet <- function (alpha)
{
  components <- length(alpha)
  x <-
    matrix(stats::rgamma(components, alpha),
           ncol = components,
           byrow = TRUE)
  sm <- x %*% rep(1, components)
  out <- x / as.vector(sm)

  return(out)
}
