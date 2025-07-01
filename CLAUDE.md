# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**intervalpsych** is an R package for analyzing interval responses in psychometrics. It implements the Interval Consensus Model (ICM) using Stan for Bayesian estimation, along with transformation and visualization functions for interval-valued data.

## Development Commands

### Testing
```bash
# Run all tests
Rscript -e "devtools::test()"

# Run specific test file
Rscript -e "testthat::test_file('tests/testthat/test-fit_icm.R')"

# Check package
Rscript -e "devtools::check()"
```

### Building and Installation
```bash
# Install package locally
Rscript -e "devtools::install()"

# Build package
Rscript -e "devtools::build()"

# Load package for development
Rscript -e "devtools::load_all()"
```

### Documentation
```bash
# Generate documentation
Rscript -e "devtools::document()"

# Build pkgdown site
Rscript -e "pkgdown::build_site()"

# Build vignettes
Rscript -e "devtools::build_vignettes()"
```

### Vignette-less Articles Workflow
This package uses a vignette-less articles workflow for pkgdown as described in: https://fml-fam.github.io/blog/2020/06/23/vignette-less-articles-with-pkgdown/

**Key Components:**
- Source vignettes stored in `vignettes/src/` directory
- `vignettes/` directory added to `.Rbuildignore`
- Articles built manually using `rmarkdown::render()` with specific format settings
- Provides complete control over when documents are rebuilt

**Build Process:**
```bash
# Rebuild vignette source files (if using Makefile)
make vignettes

# Generate pkgdown articles
make articles  

# Generate full site documentation
make docs
```

**Manual Vignette Building:**
```r
# Example render configuration used in this package
rmarkdown::render(
  input = "vignettes/src/_01-Interval-Consensus-Model.Rmd",
  output_format = rmarkdown::md_document(
    variant = "gfm",
    preserve_yaml = TRUE,
    ext = ".Rmd"
  )
)
```

## Architecture

### Core Components

1. **Stan Models** (`inst/stan/`): Two Stan model files implementing the Interval Consensus Model:
   - `icm_ilr.stan`: ICM with Isometric Log-Ratio transformation
   - `icm_slr.stan`: ICM with Sum Log-Ratio transformation
   - Both include transformation functions between simplex and bivariate normal representations

2. **Model Fitting** (`R/fit_icm.R`): Main wrapper function `fit_icm()` for fitting ICM using rstan
   - Supports both ILR and SLR link functions via `link` parameter
   - Returns `icm_stanfit` objects with components: `stan_model`, `stan_fit`, `stan_data`, `item_labels`
   - Comprehensive input validation and data checking

3. **Data Transformations** (`R/transformation_functions.R`): 
   - `ilr()`/`inv_ilr()`: Isometric log-ratio transformations
   - `slr()`/`inv_slr()`: Sum log-ratio transformations  
   - `itvl_to_splx()`/`splx_to_itvl()`: Interval bounds to simplex conversions
   - `remove_zeros()`: Handle zero components in simplex data with padding

4. **Analysis Functions**:
   - `extract_consensus()` (`R/extract_consensus.R`): Extract consensus intervals from fitted models
   - `summary.icm_stanfit()` (`R/summary_icm_stanfit.R`): Summary method for model objects

5. **Visualization** (`R/plot_*.R`): Specialized plotting functions using ggplot2/ggdist:
   - `plot_consensus()`: Plot consensus intervals from fitted ICM models
   - `plot.icm_stanfit()`: S3 method for plotting model results
   - `plot_intervals()`: General interval plotting
   - `plot_intervals_cumulative()`: Cumulative interval plots
   - `theme_icm()`: Custom ggplot2 theme

6. **Data Validation** (`R/check_data_format.R`): Helper functions for input validation:
   - `check_simplex()`: Validate simplex constraints
   - `check_bvn()`: Validate bivariate normal vectors
   - `check_interval_bounds()`: Validate interval bounds data

7. **Stan Integration** (`R/stanmodels.R`, `src/`): Generated Stan model objects and compiled C++ code

### Data Flow

1. Raw interval data → Simplex representation (`itvl_to_splx()`)
2. Remove zero components (`remove_zeros()`) if needed
3. Simplex data → ILR/SLR transformation → Bayesian estimation via Stan (`fit_icm()`)
4. Extract consensus intervals (`extract_consensus()`)
5. Visualization (`plot_consensus()`) and summarization (`summary()`)

### Key Function Signatures

- `fit_icm(df_simplex, id_person, id_item, item_labels = NULL, link = "ilr", padding = 0, iter_sampling = 500, iter_warmup = 500, n_chains = 4, n_cores = 1, adapt_delta = 0.9, ...)`
- `extract_consensus(icm_stanfit, print_summary = TRUE)` 
- `plot_consensus(icm_stanfit, method = "median_bounds", CI = 0.95)`

### Dependencies

- **Stan ecosystem**: rstan, rstantools, StanHeaders, RcppParallel for Bayesian modeling
- **Data manipulation**: dplyr, purrr for data processing
- **Visualization**: ggplot2, ggdist, ggokabeito for plotting
- **Statistical**: posterior for post-processing
- **R Core**: methods, Rcpp, RcppEigen, BH for C++ integration
- **Suggested**: testthat (>= 3.0.0) for testing

## Testing Framework

Uses testthat (edition 3) with comprehensive test coverage. Test files in `tests/testthat/` follow pattern `test-*.R` matching source files in `R/`:

- `test-fit_icm.R`: Model fitting with synthetic data
- `test-transformation_functions.R`: Log-ratio and interval transformations
- `test-extract_consensus.R`: Consensus interval extraction
- `test-plot_*.R`: Visualization functions
- `test-check_data_format.R`: Input validation
- `test-remove_zeros.R`: Zero handling
- `test-summary_icm_stanfit.R`: Model summarization
- `test-theme_icm.R`: Custom plotting theme

## Package Structure

- `R/`: Main package functions (14 files)
- `inst/stan/`: Stan model files (`icm_ilr.stan`, `icm_slr.stan`)
- `src/`: Generated C++ code from Stan models and RcppExports
- `data/`: Example datasets (`quantifiers.rda`, `quantifiers_original.rds`)
- `man/`: Generated documentation files
- `vignettes/src/`: Package vignette (`_01-Interval-Consensus-Model.Rmd`)
- `tests/testthat/`: Unit tests (11 test files)
- `docs/`: Generated pkgdown documentation website

## Stan Model Development

When modifying Stan models:
1. Edit files in `inst/stan/` directory
2. Recompile with `devtools::load_all()` or reinstall package
3. Stan models are automatically compiled on package load
4. Access compiled model objects via `stanmodels$icm_ilr` or `stanmodels$icm_slr`
5. Both models support padding for zero-component handling
6. Models include posterior predictive checks and consensus parameter extraction

## Development Workflow

- **Website and Article Build Workflow**:
  - To build the articles and the pkgdown website, I used this workflow: https://fml-fam.github.io/blog/2020/06/23/vignette-less-articles-with-pkgdown/
  - run the Makefile in "docs" directory to build the articles and the pkgdown website
