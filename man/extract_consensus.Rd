% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_consensus.R
\name{extract_consensus}
\alias{extract_consensus}
\title{Extract Consensus intervals from ICM Stan Fit Object}
\usage{
extract_consensus(icm_stanfit, print_summary = TRUE)
}
\arguments{
\item{icm_stanfit}{An object of class \code{icm_stanfit} containing the fitted Stan model.}

\item{print_summary}{A logical value indicating whether to print a summary of the extracted parameters. Default is \code{TRUE}.}
}
\value{
A list containing:
\item{df_rvar}{A data frame with extracted posterior samples as random variables.}
\item{summary}{A summary data frame with median and credible intervals for the extracted parameters.}
}
\description{
This function extracts parameter estimates for the consensus intervals from a
fitted Interval Consensus Model Stan fit object of class \code{icm_stanfit}.
}
\details{
This function extracts parameter estimates for the consensus intervals from a
fitted Interval Consensus Model Stan fit object of class \code{icm_stanfit}.
}
\examples{
\dontrun{
# Fit ICM model first
df_simplex <- data.frame(matrix(runif(100), nrow=10))
id_person <- rep(1:5, each=2)
id_item <- rep(1:2, times=5)
fit <- fit_icm(df_simplex, id_person, id_item)

# Extract consensus intervals
consensus <- extract_consensus(fit)
}

}
