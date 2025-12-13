# Fit the Interval Consensus Model

This function fits the Interval Consensus Model (ICM, Kloft et al.,
2024) using Stan.

## Usage

``` r
fit_icm(
  df_simplex,
  id_person,
  id_item,
  item_labels = NULL,
  link = "ilr",
  padding = 0,
  iter_sampling = 500,
  iter_warmup = 500,
  n_chains = 4,
  n_cores = 1,
  adapt_delta = 0.9,
  ...
)
```

## Arguments

- df_simplex:

  A dataframe containing the simplex data.

- id_person:

  A vector of person indices.

- id_item:

  A vector of item indices.

- item_labels:

  A vector of item labels. Can be long format matching id_item or a
  vector of unique labels in ascending order. Default is NULL.

- link:

  A character string specifying the link function. Options are "ilr"
  (Isometric Log-Ratio) or "slr" (Sum Log-Ratio). See also
  [`ilr()`](https://matthiaskloft.github.io/intervalpsych/reference/log_ratio_transformations.md)
  and
  [`slr()`](https://matthiaskloft.github.io/intervalpsych/reference/log_ratio_transformations.md)
  for details. Default is "ilr".

- padding:

  Padding constant that was used to remove zero-components from the
  simplex. Default is 0. The model will reverse the padding when
  transforming results back to the original interval response scale. See
  also
  [`remove_zeros()`](https://matthiaskloft.github.io/intervalpsych/reference/remove_zeros.md)
  for details.

- iter_sampling:

  An integer specifying the number of sampling iterations used by
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  Default is 500.

- iter_warmup:

  An integer specifying the number of warmup iterations used by
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  Default is 500.

- n_chains:

  An integer specifying the number of Markov chains used by
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  Default is 4.

- n_cores:

  An integer specifying the number of cores to use used by
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  Default is 1.

- adapt_delta:

  A numeric value specifying the target acceptance rate used by
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  Default is 0.9.

- ...:

  Additional arguments passed to the
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  function.

## Value

A fitted Stan model object of class `icm_stanfit` containing the
following components:

- stan_model:

  The compiled Stan model object.

- stan_fit:

  The fitted Stan model with posterior samples for the model parameters
  (see below).

- stan_data:

  The data list passed to Stan.

- item_labels:

  A vector of item labels.

The `stan_fit` component contains posterior samples for these ICM
parameters:

- Person Parameters::

  - `E_loc` - Person proficiency for location

  - `E_wid` - Person proficiency for width

  - `a_loc` - Person scaling bias for location

  - `b_loc` - Person shifting bias for location

  - `b_wid` - Person shifting bias for width

  - `rho_E` - Correlation between person proficiencies for location and
    width

- Item Parameters::

  - `Tr_loc` - Item consensus location in the logit-transformed space

  - `Tr_wid` - Item consensus width in the logit-transformed space

  - `Tr_loc_splx` - Item consensus location in the simplex space

  - `Tr_wid_splx` - Item consensus width in the simplex space

  - `Tr_L` - Item consensus lower bound

  - `Tr_U` - Item consensus upper bound

  - `Tr_splx` - Item consensus simplex representation

  - `lambda_loc` - Item discernibility for location

  - `lambda_wid` - Item discernibility for width

  - `omega` - Item residual correlations between location and width

  - `rho_lambda` - Correlation between item discernibilities for
    location and width

- Hyperparameters::

  - `mu_E` - Means for person proficiencies

  - `sigma_I` - Standard deviations for person parameters

  - `sigma_lambda` - Standard deviations for item discernibilities

- Posterior Predictive Checks::

  - `Y_ppc_loc` - Predicted responses for location in the
    logit-transformed space

  - `Y_ppc_wid` - Predicted responses for width in the logit-transformed
    space

  - `Y_ppc_splx` - Predicted responses in simplex space

  - `Y_ppc_loc_splx` - Predicted location responses in the simplex space
    (mean of lower and upper bound)

  - `Y_ppc_wid_splx` - Predicted width responses in the simplex space

## References

Kloft, M., Siepe, B. S., & Heck, D. W. (2024). The Interval Truth Model:
A Consensus Model for Continuous Bounded Interval Responses.
[doi:10.31234/osf.io/dzvw2](https://doi.org/10.31234/osf.io/dzvw2)

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

# Fit ICM model (reduce iterations for faster example)
fit <- fit_icm(df_simplex, id_person, id_item, n_chains = 1,
               iter_sampling = 100, iter_warmup = 100,
               refresh = 0)
#> Warning: There were 3 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.13, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess

# Print summary of the fit
summary(fit)
#>   T_L_median T_L_CI_025 T_L_CI_975 T_U_median T_U_CI_025 T_U_CI_975
#> 1  0.1935564 0.07474671  0.4182611  0.5627983  0.2197616  0.7832223
#> 2  0.4289527 0.20590911  0.6126743  0.6611914  0.4617445  0.8037751
# }
```
