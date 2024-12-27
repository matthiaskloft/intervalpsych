
<!-- README.md is generated from README.Rmd. Please edit that file -->

# intervalpsych

<!-- badges: start -->
<!-- badges: end -->

The goal of intervalpsych is to help you with the analysis of interval
responses.

## Installation

You can install the latest version of intervalpsych from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("matthiaskloft/intervalpsych")
```

# Fit the Interval Truth Model to the Verbal Quantifiers Dataset

First, we load the verbal quantifiers dataset:

``` r
library(intervalpsych)

data(quantifiers)
head(quantifiers)
#> # A tibble: 6 × 10
#>   id_person id_item name_ger   name_en truth scale_min scale_max width_min   x_L
#>       <int>   <int> <chr>      <chr>   <dbl>     <dbl>     <dbl>     <dbl> <dbl>
#> 1         1       1 Fuenfzig-… fifty-…    50         0       100         0    50
#> 2         1       2 ab und zu  now an…    NA         0       100         0     6
#> 3         1       3 eventuell  possib…    NA         0       100         0    15
#> 4         1       4 fast immer almost…    NA         0       100         0    89
#> 5         1       5 fast nie   almost…    NA         0       100         0     4
#> 6         1       6 gelegentl… occasi…    NA         0       100         0    16
#> # ℹ 1 more variable: x_U <dbl>
```

First, we need to convert the interval responses to the simplex format:

``` r
quantifiers <- cbind(
  quantifiers, 
  itvl_to_splx(quantifiers[,c("x_L","x_U")], min = quantifiers$scale_min, max = quantifiers$scale_max)) |> 
  na.omit()


head(quantifiers[,9:13])
#>    x_L x_U  x_1  x_2  x_3
#> 1   50  53 0.50 0.03 0.47
#> 8   99  99 0.99 0.00 0.01
#> 12   1   1 0.01 0.00 0.99
#> 17  50  50 0.50 0.00 0.50
#> 24 100 100 1.00 0.00 0.00
#> 28   0   0 0.00 0.00 1.00
```

Let’s check if we can apply the Isometric Log-Ratio transformation:

``` r
try(ilr(quantifiers[,c("x_1","x_2","x_3")]))
#> Error in check_simplex(simplex[i, ], n_elements) : 
#>   Error: None of the elements in the (row-)vector must be exactly 0! Please apply padding first!
```

It seems we have components in our simplex data that are zero. So we
first have to deal with these zero components. We can do this by adding
a padding constant:

``` r
df <- (quantifiers[,c("x_1","x_2","x_3")] + .01 ) / 1.03
head(df)
#>            x_1         x_2         x_3
#> 1  0.495145631 0.038834951 0.466019417
#> 8  0.970873786 0.009708738 0.019417476
#> 12 0.019417476 0.009708738 0.970873786
#> 17 0.495145631 0.009708738 0.495145631
#> 24 0.980582524 0.009708738 0.009708738
#> 28 0.009708738 0.009708738 0.980582524
```

``` r
fit_itm <-
  fit_itm(
    df_simplex = df,
    id_person = quantifiers$id_person,
    id_item = quantifiers$id_item,
    n_chains = 2,
    n_cores = 2,
    iter_sampling = 500,
    iter_warmup = 500
  )
#> Warning: There were 67 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 933 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
#> https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is NA, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
```
