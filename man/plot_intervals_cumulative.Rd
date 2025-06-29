% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_intervals_cumulative.R
\name{plot_intervals_cumulative}
\alias{plot_intervals_cumulative}
\title{Plot Cumulative Intervals}
\usage{
plot_intervals_cumulative(
  lower,
  upper,
  cluster_id,
  truth = NA,
  min,
  max,
  facet_wrap = NULL,
  weighted = NULL,
  show_quantiles = TRUE,
  ncol = 3
)
}
\arguments{
\item{lower}{A numeric vector of lower bounds.}

\item{upper}{A numeric vector of upper bounds.}

\item{cluster_id}{A vector of cluster IDs corresponding to the intervals.}

\item{truth}{A numeric vector of ground truth values. Default is NA.}

\item{min}{The minimum value for the x-axis.}

\item{max}{The maximum value for the x-axis.}

\item{facet_wrap}{A logical value indicating whether to use facet wrapping. Default is NULL.}

\item{weighted}{An optional vector of weights for the intervals.}

\item{show_quantiles}{A logical value indicating whether to show quantiles on the plot. Default is TRUE.}

\item{ncol}{The number of columns for facet wrapping. Default is 3.}
}
\value{
A ggplot object representing the cumulative interval plot.
}
\description{
This function generates a cumulative interval plot based on the provided lower and upper bounds, cluster IDs, and other optional parameters.
}
\examples{
# Example data
lower_bounds <- c(0.1, 0.3, 0.2)
upper_bounds <- c(0.5, 0.7, 0.6)
cluster_ids <- c(1, 1, 2)
truth_values <- c(0.3, 0.5, 0.4)

# Create cumulative interval plot
plot_intervals_cumulative(
  lower = lower_bounds,
  upper = upper_bounds,
  cluster_id = cluster_ids,
  truth = truth_values,
  min = 0,
  max = 1,
  weighted = TRUE
)

}
