#' Remove Zeros from Simplex
#'
#' Remove zero-components from interval data in the simplex format.
#'
#' @param simplex A numeric simplex vector of length 3 where the elements sum to
#' 1, or a numeric matrix or data frame where each row is a simplex vector.
#'
#' @param method A character string specifying the method to remove zeros.
#' Currently, only "rescaling" is supported. Default is "rescaling".
#'
#' @param padding A numeric value to add to each element of the simplex when
#' using the "rescaling" method. Default is 0.01.
#'
#'
#' @return A numeric vector, matrix, or data frame with the same dimensions and
#' format as the input `simplex`, with zeros removed according to the specified
#' method. The output format mirrors the input format: vector input returns a
#' vector, matrix input returns a matrix, and data frame input returns a data
#' frame.
#'
#'
#' @details
#'
#' **Rescaling**
#'
#' The rescaling methods adds a small value (`padding`) to each element of the
#' `simplex` and then divides by the row sum to close the composition.
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
    stop("Error: method must be one of ",
         paste(available_methods, collapse = ", "))
  }


  ### Track input format -------------------------------------------------------

  input_is_vector <- is.vector(simplex) && !is.list(simplex)
  input_is_dataframe <- is.data.frame(simplex)


  ### Coerce to Matrix ---------------------------------------------------------

  if (input_is_vector) {
    simplex <- t(as.matrix(simplex))
  }

  if (input_is_dataframe) {
    simplex <- as.matrix(simplex)
  }


  ### Data Checks --------------------------------------------------------------

  # check if simplex is numeric
  if (is.numeric(simplex) == FALSE) {
    stop("Error: simplex must be numeric!")
  }

  for (i in seq_len(nrow(simplex))) {
    # check that simplex is on the unit scale
    if (any(simplex[i, ] < 0) || any(simplex[i, ] > 1)) {
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

    ### Convert back to original format ----------------------------------------
    if (input_is_vector) {
      result <- as.vector(simplex[1, ])
      names(result) <- colnames(simplex)
      return(result)
    } else if (input_is_dataframe) {
      return(as.data.frame(simplex))
    } else {
      return(simplex)
    }
  }
}
