% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_consensus.R
\name{plot_consensus}
\alias{plot_consensus}
\title{Plot ICM Consensus Intervals}
\usage{
plot_consensus(icm_stanfit, method = "median_bounds", CI = 0.95)
}
\arguments{
\item{icm_stanfit}{An object of class 'icm_stanfit' containing the Stanfit results.}

\item{method}{A character string specifying the plotting method. Options are "median_bounds" (default) or "draws_distribution".}

\item{CI}{A numeric value specifying the confidence interval for the "draws_distribution" method. Default is 0.95.
This can also be a vector of length 2 for multiple confidence intervals.}
}
\value{
A ggplot2 object representing the interval plot.
}
\description{
This function generates plots for ICM Stanfit results using either the median bounds method or the draws gradient method.
}
\details{
The function checks if the input object is of class 'icm_stanfit'. It then extracts the draws for the lower and upper bounds (T_L and T_U) from the Stanfit object.

If the method is "median_bounds", the function calculates the median of the lower and upper bounds and generates an interval plot using these medians.

If the method is "draws_distribution", the function checks the number of draws and warns if it is less than 1000. It then computes a consensus distribution for each item and generates a gradient interval plot.
}
\examples{
\donttest{
# Create minimal example data
df_simplex <- data.frame(
  x1 = c(0.3, 0.4, 0.2, 0.5),
  x2 = c(0.3, 0.2, 0.4, 0.2), 
  x3 = c(0.4, 0.4, 0.4, 0.3)
)
id_person <- c(1, 1, 2, 2)
id_item <- c(1, 2, 1, 2)

# Fit ICM model
fit <- fit_icm(df_simplex, id_person, id_item, 
               iter_sampling = 100, iter_warmup = 100)

# Plot consensus intervals using median bounds
plot_consensus(fit, method = "median_bounds")
}

}
