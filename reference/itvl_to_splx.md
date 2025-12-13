# Convert from interval bounds to simplex

Convert interval responses from interval bounds format to
compostional/simplex format. See also
[`splx_to_itvl()`](https://matthiaskloft.github.io/intervalpsych/reference/splx_to_itvl.md)
for the inverse transformation.

## Usage

``` r
itvl_to_splx(interval_bounds, min = NULL, max = NULL)
```

## Arguments

- interval_bounds:

  A vector of length 2 representing the lower and upper bounds of an
  interval response or a data frame where each row contains such a
  vector.

- min:

  Minimum of the original response scale.

- max:

  Maximum of the original response scale.

## Value

A numeric vector representing a 2-simplex if input is a vector, or a
data frame where each row is a 2-simplex if input is a data frame.

## See also

[`splx_to_itvl()`](https://matthiaskloft.github.io/intervalpsych/reference/splx_to_itvl.md)

## Examples

``` r
interval_responses <- data.frame(rbind(c(.1,.5), c(.4,.7)))
itvl_to_splx(interval_responses, min = 0, max = 1)
#>   x_1 x_2 x_3
#> 1 0.1 0.4 0.5
#> 2 0.4 0.3 0.3
```
