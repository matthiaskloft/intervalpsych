#' Remove Zeros from Simplex
#'
#' This function removes zeros from a simplex (a matrix or vector where each row sums to one and all elements are between 0 and 1) using a specified method.
#'
#' @param simplex A numeric matrix, data frame, or vector representing the simplex. Each row should sum to one and all elements should be between 0 and 1.
#' @param method A character string specifying the method to remove zeros. Currently, only "rescaling" is supported. Default is "rescaling".
#' @param padding A numeric value to add to each element of the simplex when using the "rescaling" method. Default is 0.01.
#'
#' @return A numeric matrix with the same dimensions as the input `simplex`, with zeros removed according to the specified method.
#'
#' @details
#' The rescaling methods adds a small value (padding) to each element of the simplex and then rescales the rows so that they still sum to one:
# \deqn{simplex' = \frac{simplex + padding}{rowSums(simplex + padding)}}
#'
#' @examples
#' # Example usage:
#' simplex <- matrix(c(0.2, 0.3, 0.5, 0, 0.5, 0.5), nrow = 2, byrow = TRUE)
#' remove_zeros(simplex)
#'
#' @export
remove_zeros <- function(simplex,
                         method = "rescaling",
                         padding = .01) {

  ### Check method

  available_methods <- c("rescaling")

  if (!method %in% available_methods) {
    stop("Error: method must be one of ", paste(available_methods, collapse = ", "))
  }


  ### Coerce to Matrix ---------------------------------------------------------

  if (is.vector(simplex)) {
    simplex <- t(as.matrix(simplex))
  }

  if (is.data.frame(simplex)) {
    simplex <- as.matrix(simplex)
  }


  ### Data Checks --------------------------------------------------------------

  # check if simplex is numeric
  if (is.numeric(simplex) == FALSE) {
    stop("Error: simplex must be numeric!")
  }

  for (i in 1:nrow(simplex)) {

    # check that simplex is on the unit scale
    if (any(simplex[i, ] < 0) | any(simplex[i, ] > 1)) {
      stop("Error: simplex must be in the unit scale!")
    }

    # check that all elements sum to one
    if (round(sum(simplex[i, ]), 6) != 1) {
      stop("Error: simplex must sum to 1!")
    }

  }


  ### Remove Zeros -------------------------------------------------------------

  # simple replacement
  if (method == "rescaling") {
    simplex <- (simplex + padding)
    simplex <- simplex / rowSums(simplex)

    return(simplex)
  }
}
