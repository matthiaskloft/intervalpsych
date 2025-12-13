# Convert from simplex to interval bounds

Convert from simplex/compostional format to interval bounds format. See
also
[`itvl_to_splx()`](https://matthiaskloft.github.io/intervalpsych/reference/itvl_to_splx.md)
for the inverse transformation.

## Usage

``` r
splx_to_itvl(simplex, min = NULL, max = NULL)
```

## Arguments

- simplex:

  A numeric vector that is a 2-simplex (3 elements that sum to 1) or a
  data frame where each of the rows is a 2-simplex.

- min:

  Minimum of the original response scale.

- max:

  Maximum of the original response scale.

## Value

A numeric vector with 2 elements representing the lower and upper bounds
of the interval response, or a data frame where each of the rows
contains such a vector.

## See also

[`itvl_to_splx()`](https://matthiaskloft.github.io/intervalpsych/reference/itvl_to_splx.md)

## Examples

``` r
responses <- data.frame(rbind(c(.1,.5,.4), c(.3,.4,.3)))
splx_to_itvl(responses, min = 0, max = 1)
#>   x_lo x_up
#> 1  0.1  0.6
#> 2  0.3  0.7

```
