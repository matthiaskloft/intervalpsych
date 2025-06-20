
<!-- README.md is generated from README.Rmd. Please edit that file -->

# intervalpsych

<!-- badges: start -->

<!-- badges: end -->

The **intervalpsych** package provides a toolbox for the analysis of
interval responses in psychometrics and similar disciplines. Interval
responses can be used to represent uncertainty or variability in
responses to survey items, for example. Besides transformation and
plotting function, the package also contains a wrapper to estimate the
Interval Consensus Model (ICM; [Kloft, Siepe & Heck,
2024](https://doi.org/10.31234/osf.io/dzvw2)) in the probabilistic
programming language [**Stan**](https://mc-stan.org/). Results of the
model can be visualized and summarized using the functions provided in
the package.

## Installation

You can install the released version of **intervalpsych** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("intervalpsych")
```

You can install the latest development version of **intervalpsych** from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("matthiaskloft/intervalpsych")
```
