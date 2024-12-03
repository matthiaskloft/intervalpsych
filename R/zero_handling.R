#'
#' Remove zeros by rescaling or replacement
#'
#'
#' @param data Either simplex or interval data.
#'
#' @param method Method for handling zeros. You can choose between "rescale"
#' (Verkuilen & Smithson, 2012; Aitichson, 1986),"replace_simple"
#' (Martin-Fernandez et al. 2003), and "replace_multiplicative" (default; Martin-Fernandez et al. 2003).
#'
#' @param epsilon Replacement value or factor for rescaling, depending on method used.
#'
#'
#' @details
#' ## Rescaling
#' x_new = epsilon + (1 - epsilon * 2) * x_old
#' ## Simple replacement
#' Zeros are replaced by epsilon while ones are replaced by (1 - epsilon)
#' ## Multiplicative replacement
#' Placeholder
#'
#' @return Data not containing any zero values.
#' @export
#'
#'
#'

# Main user facing functions ===================================================

zero_handling <- function(data = NULL,
                          method = "replace_multiplicative",
                          epsilon = .01) {
  # checks ---------------------------------------------------------------------
  # If data is NULL
  if (is.null(data))
    stop("Please specify the input data!")
  # check simplex
  if (sum(data) != 1)
    stop("Data must be in simplex format, i.e., all values of one response must sum to one!")
  # check if data are in [0,1]
  if (min(data) < 0 &
      max(data) > 1)
    c()
  # check valid method
  if (!method %in% c("rescale", "replace_simple", "replace_multiplicative")) {
    stop("Please specify a valid method.")
  }

  # compute transformed values -------------------------------------------------
  if (method == "rescale") {
    out <- rescale(data)
  }
  if (method == "replace_simple") {
    out <- replace_simple(data)
  }
  if (method == "replace_multiplicative") {
    out <- replace_multiplicative(data)
  }

  return(out)
}




# Helper Functions =============================================================


# Rescaling (Smithson & Broomell, 2022; Verkuilen & Smithson, 2012; Aitichson, 1986) ----------------------
rescale <- function(simplex, rescaling_factor = .01){

  # add constant and rescale top end
  simplex_rescaled <- rescaling_factor / 2 + (1 - rescaling_factor) * simplex
  simplex_rescaled <- simplex_rescaled / sum(simplex_rescaled)
  return(simplex_rescaled)

}

# Simple Replacement (Smithson & Broomell, 2022; VMartin-Fernandez et al. 2003) ----------------------------
replace_simple <- function(simplex, replacement_value = .01){

  simplex_replaced <- simplex
  # replace zeros
  simplex_replaced[which(simplex == 0)] <- replacement_value
  # replace ones
  simplex_replaced <- simplex_replaced / sum(simplex_replaced)
  # reclose composition

  return(simplex_replaced)

}

# Multiplicative Replacement (Smithson & Broomell, 2022; VMartin-Fernandez et al. 2003) --------------------
replace_multiplicative <- function(simplex, replacement_value = .01){

  # number of zeros
  Z <- length(which(simplex == 0))

  simplex_replaced <- simplex
  # replace zeros
  simplex_replaced[which(simplex == 0)] <- replacement_value
  # replace ones
  simplex_replaced[which(simplex == 1)] <- 1 - Z * replacement_value

  return(simplex_replaced)

}





