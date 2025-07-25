% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_zeros.R
\name{remove_zeros}
\alias{remove_zeros}
\title{Remove Zeros from Simplex}
\usage{
remove_zeros(simplex, method = "rescaling", padding = 0.01)
}
\arguments{
\item{simplex}{A numeric simplex vector of length 3 where the elements sum to
1, or a numeric matrix or data frame where each row is a simplex vector.}

\item{method}{A character string specifying the method to remove zeros.
Currently, only "rescaling" is supported. Default is "rescaling".}

\item{padding}{A numeric value to add to each element of the simplex when
using the "rescaling" method. Default is 0.01.}
}
\value{
A numeric matrix with the same dimensions as the input \code{simplex},
with zeros removed according to the specified method.
}
\description{
Remove zero-components from interval data in the simplex format.
}
\details{
\strong{Rescaling}

The rescaling methods adds a small value (\code{padding}) to each element of the
\code{simplex} and then divides by the row sum to close the composition.
}
\examples{
# Example usage:
simplex <- matrix(c(0.2, 0.3, 0.5, 0, 0.5, 0.5), nrow = 2, byrow = TRUE)
remove_zeros(simplex)

}
