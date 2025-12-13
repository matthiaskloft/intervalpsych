# Extract consensus intervals from ICM Stan Fit Object

This function extracts parameter estimates for the consensus intervals
from a fitted Interval Consensus Model Stan fit object of class
`icm_stanfit`.

## Usage

``` r
extract_consensus(icm_stanfit, print_summary = TRUE)
```

## Arguments

- icm_stanfit:

  An object of class `icm_stanfit` containing the fitted Stan model.

- print_summary:

  A logical value indicating whether to print a summary of the extracted
  parameters. Default is `TRUE`.

## Value

A list containing:

- df_rvar:

  A data frame with extracted posterior samples in the random variable
  datatype (see
  [`posterior::rvar()`](https://mc-stan.org/posterior/reference/rvar.html)).

- summary:

  A table with posterior medians and credible intervals for the
  consensus intervals.

## Details

This function extracts parameter estimates for the consensus intervals
from a fitted Interval Consensus Model Stan fit object of class
`icm_stanfit`.

## Examples

``` r
# \donttest{
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
#> Warning: There were 9 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.08, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess

# Extract consensus intervals
consensus <- extract_consensus(fit)
#>   T_L_median T_L_CI_025 T_L_CI_975 T_U_median T_U_CI_025 T_U_CI_975
#> 1       0.22       0.02       0.42       0.57       0.23       0.77
#> 2       0.44       0.24       0.69       0.65       0.46       0.85
# }
```
