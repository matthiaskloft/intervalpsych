% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_consensus.R
\name{plot_consensus}
\alias{plot_consensus}
\title{Plot ICM Consensus Intervals}
\usage{
plot_consensus(icm_stanfit, method = "median_bounds", CI = 0.95)
}
\arguments{
\item{icm_stanfit}{An object of class \code{icm_stanfit} containing the Stanfit
results.}

\item{method}{A character string specifying the plotting method.
Options are "median_bounds" (default) or "draws_distribution".}

\item{CI}{A numeric value specifying the confidence interval for the
"draws_distribution" method. Default is 0.95. This can also be a vector of
length 2 for multiple confidence intervals.}
}
\value{
A ggplot2 object depicting the consensus interval estimates.
}
\description{
Plot consensus intervals estimated by the Interval Consensus Model (ICM) via
\code{\link[=fit_icm]{fit_icm()}}.
}
\details{
If the method is "median_bounds", the function uses posterior medians for
the lower and upper bounds of the consensus intervals.

If the method is "draws_distribution", the function computes a consensus
distribution for each consensus interval by uniformly sampling one value from
the interval range for each posterior draw.
From this distribution, a density plot is generated.
As a rough guideline, the number of draws for this method should be above 1000.
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
fit <- fit_icm(df_simplex, id_person, id_item, n_chains = 1,
               iter_sampling = 100, iter_warmup = 100,
               refresh = 0)

# Plot consensus intervals using median bounds
plot_consensus(fit, method = "median_bounds")

}

}
