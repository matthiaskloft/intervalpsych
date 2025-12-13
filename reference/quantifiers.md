# Verbal Quantifier Data

A subset of data from the data collected by Kloft & Heck (2024)
containing the probability interval judgments for verbal quantifiers.
The dataset is in the long format, with responses for the lower and
upper interval bounds in separate columns.

## Usage

``` r
quantifiers
```

## Format

A data frame with 3,344 rows and 10 columns:

- id_person:

  Unique identifier for each person

- id_item:

  Unique identifier for each item

- name_ger:

  German name of the quantifier

- name_en:

  English name of the quantifier

- truth:

  True value of the quantifier if applicable

- scale_min:

  Minimum value of the response scale

- scale_max:

  Maximum value of the response scale

- width_min:

  Minimum possible interval width of the response scale

- x_L:

  Lower bound of the interval jugdment

- x_U:

  Upper bound of the interval jugdment

## Source

<https://osf.io/67vyj/>

## References

Kloft, M., & Heck, D. W. (2024). Discriminant validity of interval
response formats: Investigating the dimensional structure of interval
widths. **Educational and Psychological Measurement, 0** (0).
[doi:10.1177/00131644241283400](https://doi.org/10.1177/00131644241283400)
