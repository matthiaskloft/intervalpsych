
### check_simplex ##############################################################


test_that("check_simplex works correctly", {
  # Test for valid 3-element simplex
  expect_silent(check_simplex(c(0.2, 0.3, 0.5), n_elements = 3))

  # Test for valid 4-element simplex
  expect_silent(check_simplex(c(0.1, 0.2, 0.3, 0.4), n_elements = 4))

  # Test for missing n_elements
  expect_error(check_simplex(c(0.2, 0.3, 0.5)),
               "Please specify the number of elements in the simplex.")

  # Test for non-numeric vector
  expect_error(check_simplex(c("a", "b", "c"), n_elements = 3),
               "Error: vector must be numeric\\!")

  # Test for incorrect length (3-element simplex)
  expect_error(check_simplex(c(0.2, 0.3), n_elements = 3),
               "Error: \\(row-\\)vector must have exactly 3 elements\\!")

  # Test for incorrect length (4-element simplex)
  expect_error(check_simplex(c(0.2, 0.3, 0.5), n_elements = 4),
               "Error: \\(row-\\)vector must have exactly 4 elements\\!")

  # Test for sum not equal to 1
  expect_error(check_simplex(c(0.2, 0.3, 0.6), n_elements = 3),
               "Error: \\(row-\\)vector must sum to 1\\!")

  # Test for zero elements
  expect_error(
    check_simplex(c(0.7, 0.3, 0), n_elements = 3),
    "Error: None of the elements in the \\(row-\\)vector must be exactly 0\\! Please apply padding first\\!"
  )
})

### check_bvn ##################################################################

test_that("check_bvn works correctly", {
  # Test for valid bivariate normal vector
  expect_silent(check_bvn(c(0.5, 0.5)))

  # Test for non-numeric vector
  expect_error(check_bvn(c("a", "b")), "Error: vector must be numeric\\!")

  # Test for incorrect length
  expect_error(check_bvn(c(0.5)), "Error: \\(row-\\)vector must have exactly 2 elements\\!")
  expect_error(check_bvn(c(0.5, 0.5, 0.5)), "Error: \\(row-\\)vector must have exactly 2 elements\\!")
})


### check_interval_bounds ######################################################

test_that("check_interval_bounds works correctly", {
  # Test for valid interval bounds data
  expect_silent(check_interval_bounds(c(1, 2), min = 0, max = 3))
  expect_silent(check_interval_bounds(c(1, 2, 3), min = 0, max = 3))

  # Test for non-numeric vector
  expect_error(check_interval_bounds(c("a", "b"), min = 0, max = 3), "Error: Input must be a numeric vector\\!")

  # Test for missing min
  expect_error(check_interval_bounds(c(1, 2), max = 3), "Please specify the minimum of the original response scale.")

  # Test for missing max
  expect_error(check_interval_bounds(c(1, 2), min = 0), "Please specify the maximum of the original response scale.")

  # Test for min greater than or equal to max
  expect_error(check_interval_bounds(c(1, 2), min = 3, max = 0), "Minimum must be smaller than Maximum\\!")
  expect_error(check_interval_bounds(c(1, 2), min = 3, max = 3), "Minimum must be smaller than Maximum\\!")

  # Test for incorrect length
  expect_error(check_interval_bounds(c(1), min = 0, max = 3), "Raw data must have either 2 or 3 values per response\\!")
  expect_error(check_interval_bounds(c(1, 2, 3, 4), min = 0, max = 3), "Raw data must have either 2 or 3 values per response\\!")
})
