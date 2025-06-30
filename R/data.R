#' Verbal Quantifier Data
#'
#' A subset of data from the data collected by Kloft & Heck (2024) containing
#' the probability interval ratings for verbal quantifiers. The dataset is in
#' the long format where all responses for the lower interval bound and upper
#' interval bound, respectively, are in one column.
#'
#' @format ## `quantifiers`
#' A data frame with 3,344 rows and 8 columns:
#' \describe{
#'   \item{id_person}{Unique identifier for each person}
#'   \item{id_item}{Unique identifier for each item}
#'   \item{name_ger}{German name of the quantifier}
#'   \item{name_eng}{English name of the quantifier}
#'   \item{truth}{True value of the quantifier if applicable}
#'   \item{scale_min}{Minimum value of the scale}
#'   \item{scale_max}{Maximum value of the scale}
#'   \item{width_min}{Minimum possible width of the interval rating}
#'   \item{x_L}{Lower bound of the interval rating}
#'   \item{x_U}{Upper bound of the interval rating}
#' }
#' @references Kloft, M., & Heck, D. W. (2024). Discriminant validity of interval
#' response formats: Investigating the dimensional structure of interval widths.
#' **Educational and Psychological Measurement, 0** (0). \doi{doi:10.1177/00131644241283400}
#'
#' @source <https://osf.io/67vyj/>
"quantifiers"


