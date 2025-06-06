# error for invalid method
test_that("remove_zeros throws an error for invalid method", {
  expect_error(remove_zeros(c(0.2, 0.3, 0.5), method = "invalid_method"), "Error: method must be one of rescaling")
})


method <- "rescaling"

test_that("remove_zeros works with a vector input", {
  simplex <- c(0.2, 0.3, 0.5)

  sum <- sum(remove_zeros(simplex, method = method))
  expect_equal(sum, 1, tolerance = 1e-6)

  has_zeros <- any(remove_zeros(simplex, method = method) == 0)
  expect_equal(has_zeros, FALSE)
})

test_that("remove_zeros works with a matrix input", {
    simplex <- matrix(c(0.2, 0.3, 0.5, 0, 0.5, 0.5), nrow = 2, byrow = TRUE)

    sum <- sum(remove_zeros(simplex, method = method))
    expect_equal(sum, 2, tolerance = 1e-6)

    has_zeros <- any(remove_zeros(simplex, method = method) == 0)
    expect_equal(has_zeros, FALSE)
})

test_that("remove_zeros works with a data frame input", {
    simplex <- data.frame(a = c(0.2, 0), b = c(0.3, 0.5), c = c(0.5, 0.5))

    sum <- sum(remove_zeros(simplex, method = method))
    expect_equal(sum, 2, tolerance = 1e-6)

    has_zeros <- any(remove_zeros(simplex, method = method) == 0)
    expect_equal(has_zeros, FALSE)
})

test_that("remove_zeros throws an error for non-numeric input", {
    simplex <- matrix(c("a", "b", "c", "d"), nrow = 2, byrow = TRUE)
    expect_error(remove_zeros(simplex), "Error: simplex must be numeric!")
})

test_that("remove_zeros throws an error for input not on unit scale", {
    simplex <- matrix(c(-0.2, 0.1, 1.1, 0, 0.5, 0.5), nrow = 2, byrow = TRUE)
    expect_error(remove_zeros(simplex), "Error: simplex must be in the unit scale!")
})

test_that("remove_zeros throws an error for input rows not summing to one", {
    simplex <- matrix(c(0.2, 0.3, 0.4, 0, 0.5, 0.5), nrow = 2, byrow = TRUE)
    expect_error(remove_zeros(simplex), "Error: simplex must sum to 1!")
})
