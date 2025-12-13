# Interval Consensus Model

``` r
library(intervalpsych)
```

We want to fit the Interval Consensus Model to the Verbal Quantifiers
dataset.

First, we load the verbal quantifiers dataset:

``` r
packages <- c("dplyr", "kableExtra")
# load packages and install if not available
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}


data(quantifiers)

quantifiers <- quantifiers |>
  # exclude control items
  dplyr::filter(!name_en %in% c("always", "never", "fifty-fifty chance")) |>
  # sample 100 respondents
  dplyr::filter(id_person %in% sample(
    size = 30,
    replace = FALSE,
    unique(quantifiers$id_person)
  )) |>
  # exclude missing values
  dplyr::filter(!is.na(x_L) & !is.na(x_U)) |>
  # recompute IDs
  dplyr::mutate(
    id_person = factor(id_person) |> as.numeric(),
    id_item = factor(id_item) |> as.numeric()
  )

head(quantifiers) |> 
  kable(digits = 2) |> 
  kable_styling()
```

| id_person | id_item | name_ger     | name_en       | truth | scale_min | scale_max | width_min | x_L | x_U |
|----------:|--------:|:-------------|:--------------|------:|----------:|----------:|----------:|----:|----:|
|         1 |       1 | ab und zu    | now and then  |    NA |         0 |       100 |         0 |   6 |  22 |
|         1 |       2 | eventuell    | possibly      |    NA |         0 |       100 |         0 |  15 |  27 |
|         1 |       3 | fast immer   | almost always |    NA |         0 |       100 |         0 |  89 |  99 |
|         1 |       4 | fast nie     | almost never  |    NA |         0 |       100 |         0 |   4 |  13 |
|         1 |       5 | gelegentlich | occasionally  |    NA |         0 |       100 |         0 |  16 |  26 |
|         1 |       6 | haeufig      | frequently    |    NA |         0 |       100 |         0 |  77 |  84 |

What does the dataset look like? We can visualize the interval responses
using the `plot_intervals_cumulative` function:

``` r
plot_intervals_cumulative(
  lower = quantifiers$x_L,
  upper = quantifiers$x_U,
  min = quantifiers$scale_min,
  max = quantifiers$scale_max,
  cluster_id = quantifiers$name_en,
  weighted = TRUE
)
#> Warning: Removed 390000 rows containing missing values or values outside the scale range
#> (`geom_vline()`).
```

![Cumulative distribution of interval
responses.](../reference/figures/README-unnamed-chunk-3-1.png)

Next, we need to convert the interval responses to the simplex format:

``` r
quantifiers <- cbind(
  quantifiers, 
  itvl_to_splx(quantifiers[,c("x_L","x_U")], min = quantifiers$scale_min, max = quantifiers$scale_max))

head(quantifiers[,9:13]) |> 
  round(2) |>
  kable() |> 
  kable_styling()
```

| x_L | x_U |  x_1 |  x_2 |  x_3 |
|----:|----:|-----:|-----:|-----:|
|   6 |  22 | 0.06 | 0.16 | 0.78 |
|  15 |  27 | 0.15 | 0.12 | 0.73 |
|  89 |  99 | 0.89 | 0.10 | 0.01 |
|   4 |  13 | 0.04 | 0.09 | 0.87 |
|  16 |  26 | 0.16 | 0.10 | 0.74 |
|  77 |  84 | 0.77 | 0.07 | 0.16 |

Let’s check if we can apply the Isometric Log-Ratio transformation:

``` r
try(ilr(quantifiers[,c("x_1","x_2","x_3")]))
#> Error in check_simplex(simplex[i, ]) : 
#>   Error: None of the elements in the (row-)vector must be exactly 0! Please apply padding first!
```

It seems we have components in our simplex data that are zero. So we
first have to deal with these zero components. We can do this by adding
a padding constant:

``` r
quantifiers[, c("x_1", "x_2", "x_3")] <- 
  remove_zeros(quantifiers[, c("x_1", "x_2", "x_3")], padding = 0.01)

head(quantifiers[,9:13]) |> 
  round(2) |>
  kable() |> 
  kable_styling()
```

| x_L | x_U |  x_1 |  x_2 |  x_3 |
|----:|----:|-----:|-----:|-----:|
|   6 |  22 | 0.07 | 0.17 | 0.77 |
|  15 |  27 | 0.16 | 0.13 | 0.72 |
|  89 |  99 | 0.87 | 0.11 | 0.02 |
|   4 |  13 | 0.05 | 0.10 | 0.85 |
|  16 |  26 | 0.17 | 0.11 | 0.73 |
|  77 |  84 | 0.76 | 0.08 | 0.17 |

``` r
fit <-
  fit_icm(
    df_simplex = quantifiers[, c("x_1", "x_2", "x_3")],
    id_person = quantifiers$id_person,
    id_item = quantifiers$id_item,
    item_labels = quantifiers |> 
      dplyr::distinct(id_item, name_en) |> 
      dplyr::pull(name_en),
    link = "ilr",
    padding = .01,
    n_chains = 4,
    n_cores = 1,
    iter_sampling = 500,
    iter_warmup = 300,
    adapt_delta = .9,
    refresh = 0,
    verbose = TRUE
  )
#> 
#> CHECKING DATA AND PREPROCESSING FOR MODEL 'icm_ilr' NOW.
#> 
#> COMPILING MODEL 'icm_ilr' NOW.
#> 
#> STARTING SAMPLER FOR MODEL 'icm_ilr' NOW.
#> Warning in validityMethod(object): The following variables have undefined
#> values: Y_ppc_splx[174,1],The following variables have undefined values:
#> Y_ppc_splx[291,1],The following variables have undefined values:
#> Y_ppc_loc_splx[174],The following variables have undefined values:
#> Y_ppc_loc_splx[291]. Many subsequent functions will not work correctly.
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
```

Now we can extract the estimated cosensus intervals from the fit object.
The function returns a list containing the posterior samples and a
summary table of the consensus intervals stemming from the posterior
medians.

``` r
consensus <- extract_consensus(fit, print_summary = FALSE)
attributes(consensus)
#> $names
#> [1] "df_rvar" "summary"
```

If we want to get a summary of the consensus intervals, we can use the
`summary` function, which is a wrapper function around
`extract_consensus`.

``` r
summary(fit) |>
  round(2) |> 
  kable() |> 
  kable_styling()
```

|                  | T_L_median | T_L_CI_025 | T_L_CI_975 | T_U_median | T_U_CI_025 | T_U_CI_975 |
|:-----------------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
| now and then     |       0.16 |       0.12 |       0.21 |       0.38 |       0.32 |       0.44 |
| possibly         |       0.19 |       0.12 |       0.26 |       0.50 |       0.38 |       0.61 |
| almost always    |       0.87 |       0.83 |       0.90 |       0.99 |       0.98 |       0.99 |
| almost never     |       0.02 |       0.01 |       0.03 |       0.10 |       0.07 |       0.13 |
| occasionally     |       0.17 |       0.12 |       0.23 |       0.38 |       0.32 |       0.45 |
| frequently       |       0.65 |       0.60 |       0.70 |       0.89 |       0.85 |       0.93 |
| hardly           |       0.03 |       0.02 |       0.05 |       0.16 |       0.12 |       0.20 |
| sometimes        |       0.23 |       0.18 |       0.28 |       0.49 |       0.42 |       0.56 |
| most of the time |       0.71 |       0.65 |       0.76 |       0.93 |       0.90 |       0.96 |
| often            |       0.65 |       0.60 |       0.69 |       0.89 |       0.85 |       0.92 |
| potentially      |       0.22 |       0.14 |       0.33 |       0.64 |       0.52 |       0.76 |
| rarely           |       0.05 |       0.03 |       0.07 |       0.18 |       0.15 |       0.23 |
| constantly       |       0.80 |       0.74 |       0.85 |       0.98 |       0.96 |       0.99 |

We can also plot the estimated consensus intervals. The generic function
`plot` calls the function `plot_consensus` with the default method
`median_bounds`.

``` r
plot(fit, method = "median_bounds")
```

![Plot of the estimated consensus
intervals.](../reference/figures/README-unnamed-chunk-10-1.png)

We can call the function `plot_consensus` directly for the alternative
plotting method `draws_distribution`, which plots the consensus
intervals based on the posterior draws. For every posterior draw, a
sample is drawn from a uniform distribution using the respective
interval bounds of the posterior draw as minimum and maximum. The result
is adistribution of consensus values from the respective consensus
interval, which is visualized in the plot. The argument `CI` specifies
the credible interval for the error bars.

``` r
plot_consensus(fit, method = "draws_distribution", CI = .95)
```

![Plot of the estimated consensus
intervals.](../reference/figures/README-unnamed-chunk-11-1.png)

Using twi CIs:

``` r
plot_consensus(fit, method = "draws_distribution", CI = c(.5, .95))
```

![Plot of the estimated consensus
intervals.](../reference/figures/README-unnamed-chunk-12-1.png)
