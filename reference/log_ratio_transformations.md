# Log-Ratio transformations for interval responses

Transform interval responses from the simplex space to the unbounded
space using either Isometric Log-Ratio (ILR) or Sum Log-Ratio (SLR)
transformations, as described by Smithson & Broomell (2024). These
transformations preserve the dimensional conceptualization of the
interval responses in terms of a location and a width. See also
[`inv_ilr()`](https://matthiaskloft.github.io/intervalpsych/reference/inv_log_ratio_transformations.md),
[`inv_slr()`](https://matthiaskloft.github.io/intervalpsych/reference/inv_log_ratio_transformations.md)
for the inverse transformations.

**ILR**

The ILR transformation equations are: \$\$x\_{loc} = \sqrt{\frac{1}{2}}
\log\left(\frac{x_1}{x_3}\right)\$\$ \$\$x\_{wid} = \sqrt{\frac{2}{3}}
\log\left(\frac{x_2}{\sqrt{x_1 x_3}}\right)\$\$

**SLR**

The SLR transformation equations are: \$\$x\_{loc} =
\log\left(\frac{x_1}{x_3}\right)\$\$ \$\$x\_{wid} =
\log\left(\frac{x_2}{x_1 + x_3}\right)\$\$

where \\(x_1, x_2, x_3)\\ is the interval response in the simplex format
and \\(x\_{loc}, x\_{wid})\\ are the transformed values representing the
unbounded location and width.

## Usage

``` r
ilr(simplex)

slr(simplex)
```

## Arguments

- simplex:

  A numeric vector that is a 2-simplex (3 elements that sum to 1) or a
  dataframe where each of the rows is a 2-simplex.

## Value

A numeric vector with 2 elements, the unbounded interval location and
width, or a dataframe where each of the rows is a numeric vector with
these 2 elements.

## References

Smithson, M., & Broomell, S. B. (2024). Compositional data analysis
tutorial. Psychological Methods, 29(2), 362–378.

## See also

[`inv_ilr()`](https://matthiaskloft.github.io/intervalpsych/reference/inv_log_ratio_transformations.md),
[`inv_slr()`](https://matthiaskloft.github.io/intervalpsych/reference/inv_log_ratio_transformations.md)

## Examples

``` r
# Generate some simplex data
simplex <- data.frame(rbind(c(.1, .2, .7), c(.4, .5, .1)))

# ILR transformation
ilr(simplex)
#>        x_loc      x_wid
#> 1 -1.3759663 -0.2284622
#> 2  0.9802581  0.7481482

# SLR transformation
slr(simplex)
#>       x_loc     x_wid
#> 1 -1.945910 -1.386294
#> 2  1.386294  0.000000

```
