
#' @title Apply min-max scaling to a vector ------------------------------------
#'
#' @param vector Numeric input vector.

#' @param min Minimum possible response value (only required for interval type data).
#'
#' @param max Minimum possible response value (only required for interval type data).
#'
#'@export
min_max <- function(vector,
                    min = NULL,
                    max = NULL) {
  # check numeric
  if (!is.numeric(vector)) {
    stop("Error: Input must be a numeric vector!")
  }
  # compute min
  if (is.null(min)) {
    min <- min(vector)
    stop("Note: The minimum value was inferred from the data. Minimum = ",
            min)
  }
  # compute max
  if (is.null(max)) {
    max <- max(vector)
    stop("Note: The maximum value was inferred from the data. Maximum = ",
            max)
  }
  # check min is smaller than max
  if (min >= max) {
    stop("Minimum must be smaller than Maximum!")
  }

  # rescale data
  vectror_scaled <- (vector - min) / (max - min)


  return(vectror_scaled)
}

