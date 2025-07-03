#-------------------------------------------------------------------------------
# Log-Ratio Transformations for Interval Responses
#-------------------------------------------------------------------------------

#' @title Log-Ratio transformations for interval responses
#' @rdname log_ratio_transformations
#' @description
#' Transform interval responses from the simplex space to the unbounded space
#' using either Isometric Log-Ratio (ILR) or Sum Log-Ratio (SLR)
#' transformations, as described by Smithson & Broomell (2024).
#' These transformations preserve the dimensional conceptualization of the
#' interval responses in terms of a location and a width.
#' See also [inv_ilr()], [inv_slr()] for the inverse transformations.
#'
#' **ILR**
#'
#' The ILR transformation equations are:
#' \deqn{x_{loc} = \sqrt{\frac{1}{2}} \log\left(\frac{x_1}{x_3}\right)}
#' \deqn{x_{wid} = \sqrt{\frac{2}{3}} \log\left(\frac{x_2}{\sqrt{x_1 x_3}}\right)}
#'
#' **SLR**
#'
#' The SLR transformation equations are:
#' \deqn{x_{loc} = \log\left(\frac{x_1}{x_3}\right)}
#' \deqn{x_{wid} = \log\left(\frac{x_2}{x_1 + x_3}\right)}
#'
#' where \eqn{(x_1, x_2, x_3)} is the interval response in the simplex format
#' and \eqn{(x_{loc}, x_{wid})} are the transformed values representing the
#' unbounded location and width.
#'
#' @param simplex A numeric vector that is a 2-simplex (3 elements that sum to 1)
#' or a dataframe where each of the rows is a 2-simplex.
#'
#' @return A numeric vector with 2 elements, the unbounded interval location and
#' width, or a dataframe where each of the rows is a numeric vector with these 2
#' elements.
#'
#' @seealso [inv_ilr()], [inv_slr()]
#'
#' @export
#' @references
#' Smithson, M., & Broomell, S. B. (2024). Compositional data analysis tutorial.
#' Psychological Methods, 29(2), 362–378.
#'
#' @examples
#' # Generate some simplex data
#' simplex <- data.frame(rbind(c(.1, .2, .7), c(.4, .5, .1)))
#'
#' # ILR transformation
#' ilr(simplex)
#'
#' # SLR transformation
#' slr(simplex)
#'
#'
ilr <- function(simplex) {
  if (!is.data.frame(simplex) && !is.matrix(simplex)) {
    #### vector

    n_elements <- length(simplex)

    if (n_elements != 3) {
      stop("Simplex must have 3 elements")
    }

    # run checks
    check_simplex(simplex)

    # calculate ILR
    Y <- rep(NA, 2)
    Y[1] <- sqrt(1 / 2) * log(simplex[1] / simplex[3])
    Y[2] <- sqrt(2 / 3) * log(simplex[2] / sqrt(simplex[1] * simplex[3]))

    names(Y) <- c("x_loc", "x_wid")

    return(Y)


  } else{
    ### dataframe

    # coerce to matrix
    simplex <- as.matrix(simplex)

    # get number of cols
    n_elements <- ncol(simplex)

    if (n_elements != 3) {
      stop("Simplex must have 3 elements")
    }

    # run checks
    for (i in seq_len(nrow(simplex))) {
      check_simplex(simplex[i, ])
    }

    # calculate ILR
    Y <- apply(
      X = simplex,
      MARGIN = 1,
      FUN = function(X) {
        Y <- rep(NA, 2)
        Y[1] <- sqrt(1 / 2) * log(X[1] / X[3])
        Y[2] <- sqrt(2 / 3) * log(X[2] / sqrt(X[1] * X[3]))

        return(Y)
      },
      simplify = FALSE
    )
    Y <- do.call(what = "rbind", args = Y)

    return(data.frame(x_loc = Y[, 1], x_wid = Y[, 2]))
  }
}

## test examples
# ilr(c(.4,.2,.4))
# ilr(c(.3,.2,.5))
# ilr(c(.5,.2,.3))
# ilr(c(1/3,1/3,1/3))
# simplex <- data.frame(rbind(c(.1, .2, .7), c(.4, .5, .1)))
# ilr(simplex)



#' @title Inverse Log-Ratio transformations for interval responses
#' @rdname inv_log_ratio_transformations
#' @description
#' Transform unbounded data back to the simplex space using either Isometric Log-Ratio (ILR)
#' or Sum Log-Ratio (SLR) inverse transformations, as described by Smithson & Broomell (2024).
#' These transformations are the inverse transformations of [ilr()] and [slr()].
#'
#' **Inverse ILR**
#'
#' The inverse ILR transformation equations are:
#' \deqn{x_1 = \frac{\exp(\sqrt{2} x_{loc})}{\exp(\sqrt{2} x_{loc}) + \exp(\sqrt{\frac{3}{2}} x_{wid} + \frac{x_{loc}}{\sqrt{2}}) + 1}}
#' \deqn{x_2 = \frac{\exp(\sqrt{\frac{3}{2}} x_{wid} + \frac{x_{loc}}{\sqrt{2}})}{\exp(\sqrt{2} x_{loc}) + \exp(\sqrt{\frac{3}{2}} x_{wid} + \frac{x_{loc}}{\sqrt{2}}) + 1}}
#' \deqn{x_3 = \frac{1}{\exp(\sqrt{2} x_{loc}) + \exp(\sqrt{\frac{3}{2}} x_{wid} + \frac{x_{loc}}{\sqrt{2}}) + 1}}
#'
#' **Inverse SLR**
#'
#' The inverse SLR transformation equations are:
#' \deqn{x_1 = \frac{\exp(x_{loc})}{(\exp(x_{loc}) + 1)(\exp(x_{wid}) + 1)}}
#' \deqn{x_2 = \frac{\exp(x_{wid})}{\exp(x_{wid}) + 1}}
#' \deqn{x_3 = \frac{1}{(\exp(x_{loc}) + 1)(\exp(x_{wid}) + 1)}}
#'
#' where \eqn{(x_{loc}, x_{wid})} are the unbounded interval location and width
#' and \eqn{(x_1, x_2, x_3)} is the resulting interval response in the simplex format.
#'
#' @param bvn A numeric vector containing an unbounded interval location and width or
#' a dataframe where each of the rows consists of such a vector.
#'
#' @return A numeric vector containing a 2-simplex or a dataframe where each of
#' the rows consists of such a vector.
#'
#' @seealso [ilr()], [slr()]
#'
#' @export
#' @references
#' Smithson, M., & Broomell, S. B. (2024). Compositional data analysis tutorial. Psychological Methods, 29(2), 362–378.
#'
#' @examples
#' # Generate some unbounded data
#' bvn <- data.frame(rbind(c(0, .2), c(-2, .4)))
#'
#' # Inverse ILR transformation
#' inv_ilr(bvn)
#'
#' # Inverse SLR transformation
#' inv_slr(bvn)
#'
#'
inv_ilr <- function(bvn) {
  if (!is.data.frame(bvn) && !is.matrix(bvn)) {
    #### vector

    # run checks
    check_bvn(bvn)

    # calculate inverse ILR
    Y <- rep(NA, 3)
    Y[1] <- exp(sqrt(2) * bvn[1])
    Y[2] <- exp(sqrt(3 / 2) *  bvn[2] + bvn[1] / sqrt(2))
    Y[3] <- 1
    Y <- Y / sum(Y)

    names(Y) <- c("x_1", "x_2", "x_3")

    return(Y)

  } else {
    ### dataframe

    # coerce to matrix
    bvn <- as.matrix(bvn)

    # run checks
    for (i in seq_len(nrow(bvn))) {
      check_bvn(bvn[i, ])
    }

    # calculate inverse ILR
    Y <- apply(
      X = bvn,
      MARGIN = 1,
      FUN = function(X) {
        Y <- rep(NA, 3)
        Y[1] <- exp(sqrt(2) * X[1])
        Y[2] <- exp((sqrt(3 / 2) * X[2]) + (X[1] / sqrt(2)))
        Y[3] <- 1
        Y <- Y / sum(Y)

        names(Y) <- c("x_1", "x_2", "x_3")

        return(Y)
      },
      simplify = FALSE
    )
    Y <- do.call(what = "rbind", args = Y)

    return(data.frame(
      x_1 = Y[, 1],
      x_2 = Y[, 2],
      x_3 = Y[, 3]
    ))
  }
}

# # test examples
# inv_ilr(c(0,0))
# inv_ilr(c(1,0))
# inv_ilr(c(0,1))
# inv_ilr(c(-1,0))
#
# bvn <- data.frame(rbind(c(0, .2), c(-2, .4)))
# a <- inv_ilr(bvn)
#
# sum(inv_ilr(c(0,0)))
# sum(inv_ilr(c(1,0)))
# sum(inv_ilr(c(0,1)))


#' @rdname log_ratio_transformations
#' @export
slr <- function(simplex) {
  if (!is.data.frame(simplex) && !is.matrix(simplex)) {
    #### vector

    n_elements <- length(simplex)

    if (n_elements != 3) {
      stop("Simplex must have 3 elements")
    }

    # run checks
    check_simplex(simplex)

    # calculate SLR
    Y <- rep(NA, 2)
    Y[1] <- log(simplex[1] / simplex[3])
    Y[2] <- log(simplex[2] / (simplex[1] + simplex[3]))

    names(Y) <- c("x_loc", "x_wid")

    return(Y)


  } else{
    ### dataframe

    # coerce to matrix
    simplex <- as.matrix(simplex)

    # get number of cols
    n_elements <- ncol(simplex)

    if (n_elements != 3) {
      stop("Simplex must have 3 elements")
    }

    # run checks
    for (i in seq_len(nrow(simplex))) {
      check_simplex(simplex[i, ])
    }

    # calculate SLR
    Y <- apply(
      X = simplex,
      MARGIN = 1,
      FUN = function(X) {
        Y <- rep(NA, 2)
        Y[1] <- log(X[1] / X[3])
        Y[2] <- log(X[2] / (X[1] + X[3]))

        return(Y)
      },
      simplify = FALSE
    )
    Y <- do.call(what = "rbind", args = Y)

    return(data.frame(x_loc = Y[, 1], x_wid = Y[, 2]))
  }
}

## test examples
# slr(c(.4,.2,.4))
# slr(c(.3,.2,.5))
# slr(c(.5,.2,.3))
# slr(c(1/3,1/3,1/3))
# simplex <- data.frame(rbind(c(.1, .2, .7), c(.4, .5, .1)))
# slr(simplex)




#' @rdname inv_log_ratio_transformations
#' @export
inv_slr <- function(bvn) {
  if (!is.data.frame(bvn) && !is.matrix(bvn)) {
    #### vector

    # run checks
    check_bvn(bvn)

    # calculate inverse SLR
    Y <- rep(NA, 3)
    Y[1] <- exp(bvn[1]) / ((exp(bvn[1]) + 1) * (exp(bvn[2]) + 1))
    Y[2] <- exp(bvn[2]) /  (exp(bvn[2]) + 1)
    Y[3] <- 1 /           ((exp(bvn[1]) + 1) * (exp(bvn[2]) + 1))

    names(Y) <- c("x_1", "x_2", "x_3")

    return(Y)

  } else {
    ### dataframe

    # coerce to matrix
    bvn <- as.matrix(bvn)

    # run checks
    for (i in seq_len(nrow(bvn))) {
      check_bvn(bvn[i, ])
    }

    # calculate inverse SLR
    Y <- apply(
      X = bvn,
      MARGIN = 1,
      FUN = function(X) {
        Y <- rep(NA, 3)
        Y[1] <- exp(X[1]) / ((exp(X[1]) + 1) * (exp(X[2]) + 1))
        Y[2] <- exp(X[2]) /  (exp(X[2]) + 1)
        Y[3] <- 1 /          ((exp(X[1]) + 1) * (exp(X[2]) + 1))

        names(Y) <- c("x_1", "x_2", "x_3")

        return(Y)
      },
      simplify = FALSE
    )
    Y <- do.call(what = "rbind", args = Y)

    return(data.frame(
      x_1 = Y[, 1],
      x_2 = Y[, 2],
      x_3 = Y[, 3]
    ))
  }
}

# # test examples
# inv_slr(c(0,0))
# inv_slr(c(1,0))
# inv_slr(c(0,1))
# inv_slr(c(-1,0))
#
# bvn <- data.frame(rbind(c(0, .2), c(-2, .4)))
# a <- inv_slr(bvn)
#
# sum(inv_slr(c(0,0)))
# sum(inv_slr(c(1,0)))
# sum(inv_slr(c(0,1)))


#-------------------------------------------------------------------------------
# Interval Bounds to Simplex and Back
#-------------------------------------------------------------------------------


#' @title Convert from interval bounds to simplex
#' @description Convert interval responses from interval bounds format to
#' compostional/simplex format. See also [splx_to_itvl()] for the inverse
#' transformation.
#' @param interval_bounds A vector of length 2 representing the lower and upper
#' bounds of an interval response or a data frame where each row contains such
#' a vector.
#'
#' @param min Minimum of the original response scale.
#' @param max Maximum of the original response scale.
#'
#' @return A numeric vector representing a 2-simplex if input is a vector, or a
#' data frame where each row is a 2-simplex if input is a data frame.
#'
#' @seealso [splx_to_itvl()]
#'
#' @export
#'
#' @examples
#' interval_responses <- data.frame(rbind(c(.1,.5), c(.4,.7)))
#' itvl_to_splx(interval_responses, min = 0, max = 1)
#'
itvl_to_splx <- function(interval_bounds,
                         min = NULL,
                         max = NULL) {
  if (!is.data.frame(interval_bounds) &&
      !is.matrix(interval_bounds)) {
    ### vector
    check_interval_bounds(interval_bounds, min, max)

    # compute simplex
    if (length(interval_bounds) == 2) {
      comp <- c(
        x_1 = (interval_bounds[1] - min) / max,
        x_2 = (interval_bounds[2] - interval_bounds[1]) / max,
        x_3 = (max - interval_bounds[2]) / max
      )
    }

    return(comp)

  } else{
    ### dataframe / matrix
    # coerce to matrix
    interval_bounds <- as.matrix(interval_bounds)
    if (length(min) == 1)
      min <- rep(min, nrow(interval_bounds))
    if (length(max) == 1)
      max <- rep(max, nrow(interval_bounds))

    # run checks
    for (i in seq_len(nrow(interval_bounds))) {
      check_interval_bounds(interval_bounds[i, ], min[i], max[i])
    }

    # compute simplex
    if (ncol(interval_bounds) == 2) {
      comp <- data.frame(
        x_1 = (interval_bounds[, 1] - min) / max,
        x_2 = (interval_bounds[, 2] - interval_bounds[, 1]) / max,
        x_3 = (max - interval_bounds[, 2]) / max
      )
    }

    return(comp)
  }
}

# test examples
#itvl_to_splx(c(.1, .5), min = 0, max = 1)
#interval_responses <- data.frame(rbind(c(.1, .5), c(.4, .7)))
#itvl_to_splx(interval_responses, min = 0, max = 1)





#' @title Convert from simplex to interval bounds
#' @description Convert from simplex/compostional format to interval bounds
#' format. See also [itvl_to_splx()] for the inverse transformation.
#'
#' @param simplex A numeric vector that is a 2-simplex (3 elements that sum to 1)
#' or a data frame where each of the rows is a 2-simplex.
#'
#' @param min Minimum of the original response scale.
#' @param max Maximum of the original response scale.
#'
#' @return A numeric vector with 2 elements representing the lower and upper
#' bounds of the interval response, or a data frame where each of the rows
#' contains such a vector.
#'
#' @seealso [itvl_to_splx()]
#'
#' @export
#'
#' @examples
#' responses <- data.frame(rbind(c(.1,.5,.4), c(.3,.4,.3)))
#' splx_to_itvl(responses, min = 0, max = 1)
#'
#'
splx_to_itvl <- function(simplex, min = NULL, max = NULL) {
  if (!is.data.frame(simplex) && !is.matrix(simplex)) {
    ### vector

    n_elements <- length(simplex)

    # check that n_elements is 3
    if (n_elements != 3) {
      stop("Simplex must have 3 elements")
    }

    # run checks
    check_simplex(simplex)

    # compute simplex
    if (length(simplex) == 3) {
      interval <- c(x_lo = simplex[1] + min, x_up = max - simplex[3])
    }

    return(interval)

  } else{
    ### dataframe / matrix

    # coerce to matrix
    simplex <- as.matrix(simplex)

    n_elements <- ncol(simplex)

    # check that n_elements is 3
    if (n_elements != 3) {
      stop("Simplex must have 3 elements")
    }

    # run checks
    for (i in seq_len(nrow(simplex))) {
      check_simplex(simplex[i, ])
    }

    # compute simplex
    if (ncol(simplex) == 3) {
      interval <- data.frame(x_lo = simplex[, 1] + min, x_up = max - simplex[, 3])
    }

    return(interval)
  }
}

# # test examples
# splx_to_itvl(c(.1, .5, .4), min = 0, max = 1)
# responses <- data.frame(rbind(c(.1,.5,.4), c(.3,.4,.3)))
# splx_to_itvl(responses, min = 0, max = 1)
