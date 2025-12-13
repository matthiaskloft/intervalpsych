# Inverse Log-Ratio transformations for interval responses

Transform unbounded data back to the simplex space using either
Isometric Log-Ratio (ILR) or Sum Log-Ratio (SLR) inverse
transformations, as described by Smithson & Broomell (2024). These
transformations are the inverse transformations of
[`ilr()`](https://matthiaskloft.github.io/intervalpsych/reference/log_ratio_transformations.md)
and
[`slr()`](https://matthiaskloft.github.io/intervalpsych/reference/log_ratio_transformations.md).

**Inverse ILR**

The inverse ILR transformation equations are: \$\$x_1 =
\frac{\exp(\sqrt{2} x\_{loc})}{\exp(\sqrt{2} x\_{loc}) +
\exp(\sqrt{\frac{3}{2}} x\_{wid} + \frac{x\_{loc}}{\sqrt{2}}) + 1}\$\$
\$\$x_2 = \frac{\exp(\sqrt{\frac{3}{2}} x\_{wid} +
\frac{x\_{loc}}{\sqrt{2}})}{\exp(\sqrt{2} x\_{loc}) +
\exp(\sqrt{\frac{3}{2}} x\_{wid} + \frac{x\_{loc}}{\sqrt{2}}) + 1}\$\$
\$\$x_3 = \frac{1}{\exp(\sqrt{2} x\_{loc}) + \exp(\sqrt{\frac{3}{2}}
x\_{wid} + \frac{x\_{loc}}{\sqrt{2}}) + 1}\$\$

**Inverse SLR**

The inverse SLR transformation equations are: \$\$x_1 =
\frac{\exp(x\_{loc})}{(\exp(x\_{loc}) + 1)(\exp(x\_{wid}) + 1)}\$\$
\$\$x_2 = \frac{\exp(x\_{wid})}{\exp(x\_{wid}) + 1}\$\$ \$\$x_3 =
\frac{1}{(\exp(x\_{loc}) + 1)(\exp(x\_{wid}) + 1)}\$\$

where \\(x\_{loc}, x\_{wid})\\ are the unbounded interval location and
width and \\(x_1, x_2, x_3)\\ is the resulting interval response in the
simplex format.

## Usage

``` r
inv_ilr(bvn)

inv_slr(bvn)
```

## Arguments

- bvn:

  A numeric vector containing an unbounded interval location and width
  or a dataframe where each of the rows consists of such a vector.

## Value

A numeric vector containing a 2-simplex or a dataframe where each of the
rows consists of such a vector.

## References

Smithson, M., & Broomell, S. B. (2024). Compositional data analysis
tutorial. Psychological Methods, 29(2), 362–378.

## See also

[`ilr()`](https://matthiaskloft.github.io/intervalpsych/reference/log_ratio_transformations.md),
[`slr()`](https://matthiaskloft.github.io/intervalpsych/reference/log_ratio_transformations.md)

## Examples

``` r
# Generate some unbounded data
bvn <- data.frame(rbind(c(0, .2), c(-2, .4)))

# Inverse ILR transformation
inv_ilr(bvn)
#>          x_1       x_2       x_3
#> 1 0.30510538 0.3897892 0.3051054
#> 2 0.04059715 0.2725466 0.6868563

# Inverse SLR transformation
inv_slr(bvn)
#>         x_1       x_2       x_3
#> 1 0.2250830 0.5498340 0.2250830
#> 2 0.0478376 0.5986877 0.3534747

```
