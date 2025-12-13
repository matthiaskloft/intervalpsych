# Remove Zeros from Simplex

Remove zero-components from interval data in the simplex format.

## Usage

``` r
remove_zeros(simplex, method = "rescaling", padding = 0.01)
```

## Arguments

- simplex:

  A numeric simplex vector of length 3 where the elements sum to 1, or a
  numeric matrix or data frame where each row is a simplex vector.

- method:

  A character string specifying the method to remove zeros. Currently,
  only "rescaling" is supported. Default is "rescaling".

- padding:

  A numeric value to add to each element of the simplex when using the
  "rescaling" method. Default is 0.01.

## Value

A numeric matrix with the same dimensions as the input `simplex`, with
zeros removed according to the specified method.

## Details

**Rescaling**

The rescaling methods adds a small value (`padding`) to each element of
the `simplex` and then divides by the row sum to close the composition.

## Examples

``` r
# Example usage:
simplex <- matrix(c(0.2, 0.3, 0.5, 0, 0.5, 0.5), nrow = 2, byrow = TRUE)
remove_zeros(simplex)
#>             [,1]      [,2]      [,3]
#> [1,] 0.203883495 0.3009709 0.4951456
#> [2,] 0.009708738 0.4951456 0.4951456
```
