#' Verbal Quantifier Data
#'
#' A subset of data from the data collected by Kloft & Heck (2024) containing
#' the probability interval judgments for verbal quantifiers. The dataset is in
#' the long format, with responses for the lower and upper interval bounds in
#' separate columns.
#'
#' @format
#' A data frame with 3,344 rows and 10 columns:
#'
#' \describe{
#'   \item{id_person}{Unique identifier for each person}
#'   \item{id_item}{Unique identifier for each item}
#'   \item{name_ger}{German name of the quantifier}
#'   \item{name_en}{English name of the quantifier}
#'   \item{truth}{True value of the quantifier if applicable}
#'   \item{scale_min}{Minimum value of the response scale}
#'   \item{scale_max}{Maximum value of the response scale}
#'   \item{width_min}{Minimum possible interval width of the response scale}
#'   \item{x_L}{Lower bound of the interval jugdment}
#'   \item{x_U}{Upper bound of the interval jugdment}
#' }
#' @references Kloft, M., & Heck, D. W. (2024). Discriminant validity of
#' interval response formats: Investigating the dimensional structure of
#' interval widths. **Educational and Psychological Measurement, 0** (0).
#' \doi{doi:10.1177/00131644241283400}
#'
#' @source <https://osf.io/67vyj/>
"quantifiers"
