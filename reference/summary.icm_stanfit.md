# Summarize ICM Stanfit Object

This function provides a summary method for objects of class
`icm_stanfit`. via a wrapper around the
[`extract_consensus()`](https://matthiaskloft.github.io/intervalpsych/reference/extract_consensus.md)
function.

## Usage

``` r
# S3 method for class 'icm_stanfit'
summary(object, ...)
```

## Arguments

- object:

  An object of class `icm_stanfit`.

- ...:

  Additional arguments (currently not used).

## Value

A table with posterior medians and credible intervals for the consensus
intervals.

## See also

[`extract_consensus()`](https://matthiaskloft.github.io/intervalpsych/reference/extract_consensus.md)

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
#> Warning: There were 3 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.07, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess

# Get summary
summary(fit)
#>   T_L_median T_L_CI_025 T_L_CI_975 T_U_median T_U_CI_025 T_U_CI_975
#> 1  0.2221595 0.07444702  0.5376368  0.5685477  0.2690143  0.8265028
#> 2  0.4153712 0.21775299  0.7183315  0.6509958  0.4386705  0.8860591
# }
```
