### Helper Functions to check that data have valid formatting


# helper function: run checks on simplex----------------------------------------
#' @title Check if data is a valid simplex
#' @description Check if data is a valid simplex
#'
#' @param simplex A numeric vector that is a 2-simplex (3 elements that sum to 1)
#' or a dataframe where each of the rows is a 2-simplex
#' @param n_elements The number of elements in the simplex (3 or 4)
#'
#'
check_simplex <- function(simplex, n_elements = NULL) {

  # check if n_elements is specified
  if (is.null(n_elements)) {
    stop("Please specify the number of elements in the simplex.")
  }

  # is numeric
  if (is.numeric(simplex) == FALSE) {
    stop("Error: vector must be numeric!")
  }

  # has length 3
  if (n_elements == 3 && length(simplex) != 3) {
    stop("Error: (row-)vector must have exactly 3 elements!")
  }

  # has length 4
  if (n_elements == 4 && length(simplex) != 4) {
    stop("Error: (row-)vector must have exactly 4 elements!")
  }

  # check that all elementssum to one, round the sum to prevent numerical issues
  if (round(sum(simplex), 6) != 1) {
    stop("Error: (row-)vector must sum to 1!")
  }

  # all elements are non-zero and sums to one
  if (any(simplex == 0)) {
    stop(
      "Error: None of the elements in the (row-)vector must be exactly 0! Please apply padding first!"
    )
  }

}


# helper function: run checks on simplex----------------------------------------
#' @title Check if data is a valid bivariate normal vector
#' @description Check if data is a valid bivariate normal vector
#' @param bvn A numeric vector that is a bivariate normal vector (2 elements) or
#' a dataframe where each of the rows is a bivariate normal vector
#'
#'
check_bvn <- function(bvn) {
  # is numeric
  if (!is.numeric(bvn)) {
    stop("Error: vector must be numeric!")
  }

  # has length 2
  if (length(bvn) != 2) {
    stop("Error: (row-)vector must have exactly 2 elements!")
  }
}




# helper function: check interval bounds data ----------------------------------
check_interval_bounds <- function(raw_data,
                                  min = NULL,
                                  max = NULL) {

  # check numeric
  if (!is.numeric(raw_data)) {
    stop("Error: Input must be a numeric vector!")
  }
  # compute min
  if (is.null(min)) {
    stop("Please specify the minimum of the original response scale.",
         min)
  }
  # compute max
  if (is.null(max)) {
    stop("Please specify the maximum of the original response scale.",
         max)
  }
  # check min is smaller than max
  if (min >= max) {
    stop("Minimum must be smaller than Maximum!")
  }

  l <- length(raw_data)
  # check length
  if (!l %in% 2:3) {
    stop("Raw data must have either 2 or 3 values per response!")
  }
}
