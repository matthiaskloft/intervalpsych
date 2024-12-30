
<!-- README.md is generated from README.Rmd. Please edit that file -->

# intervalpsych

<!-- badges: start -->
<!-- badges: end -->

The **intervalpsych** package provides a toolbox for the analysis of
interval responses in psychometrics and similar disciplines. Interval
responses can be used to represent uncertainty or variability in
responses to survey items, for example. Besides transformation and
plotting function, the package also contains a wrapper to estimate the
Interval Truth Model (ITM; [Kloft, Siepe & Heck,
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

## Ressources

The best way to get started is to visit the package website and the
introductory vignette:

- Website: <https://matthiaskloft.github.io/intervalpsych/>

- Vignette: [Interval Truth
  Model](https://matthiaskloft.github.io/intervalpsych/articles/Interval-Truth-Model.html)

- [Open an Issue](https://github.com/matthiaskloft/intervalpsych/issues)

## Citation

If you want to cite the package, please use the following citation:

Kloft, M., Siepe, B. S., & Heck, D. W. (2024, October 25). The Interval
Truth Model: A Consensus Model for Continuous Bounded Interval
Responses. *PsyArXiv Preprint*. <https://doi.org/10.31234/osf.io/dzvw2>

As a BibTeX entry:

``` bibtex
@article{kloft2024,
  title={The Interval Truth Model: A Consensus Model for Continuous Bounded Interval Responses},
  author={Kloft, Matthias and Siepe, Bj{\"o}rn S. and Heck, Daniel W.},
  note={PsyArXiv Preprint},
  year={2024},
  doi={10.31234/osf.io/dzvw2},
  url={https://doi.org/10.31234/osf.io/dzvw2}
}
```
