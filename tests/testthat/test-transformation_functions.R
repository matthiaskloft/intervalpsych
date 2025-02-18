


### ILR ########################################################################

# Test for vector input
test_that("ilr transformation works for vector input", {
  simplex <- c(0.4, 0.2, 0.4)
  result <- ilr(simplex)
  expect_equal(length(result), 2)
  expect_named(result, c("x_loc", "x_wid"))
})

# Test for dataframe input
test_that("ilr transformation works for dataframe input", {
  simplex <- data.frame(rbind(c(0.1, 0.2, 0.7), c(0.4, 0.5, 0.1)))
  result <- ilr(simplex)
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 2)
  expect_named(result, c("x_loc", "x_wid"))
})

# Test for invalid input
test_that("ilr transformation handles invalid input", {
  expect_error(ilr(c(0.4, 0.2)), "Simplex must have 3 elements")
  expect_error(ilr(data.frame(rbind(
    c(0.1, 0.2), c(0.4, 0.5)
  ))), "Simplex must have 3 elements")
})


### Inverse ILR ################################################################

# Test for vector input
test_that("inv_ilr transformation works for vector input", {
  bvn <- c(0, 0.2)
  result <- inv_ilr(bvn)
  expect_equal(length(result), 3)
  expect_named(result, c("x_1", "x_2", "x_3"))
})

# Test for dataframe input
test_that("inv_ilr transformation works for dataframe input", {
  bvn <- data.frame(rbind(c(0, 0.2), c(-2, 0.4)))
  result <- inv_ilr(bvn)
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 2)
  expect_named(result, c("x_1", "x_2", "x_3"))
})

# Test for invalid input
test_that("inv_ilr transformation handles invalid input", {
  expect_error(inv_ilr(c(0.4)),
               "Error: \\(row-\\)vector must have exactly 2 elements\\!")
  expect_error(inv_ilr(data.frame(rbind(c(
    0.1
  ), c(
    0.4
  )))),
  "Error: \\(row-\\)vector must have exactly 2 elements\\!")
})


### Interval to Simplex ########################################################

# Test for vector input with 2 elements
test_that("itvl_to_splx transformation works for vector input with 2 elements",
          {
            interval_bounds <- c(0.1, 0.5)
            result <- itvl_to_splx(interval_bounds, min = 0, max = 1)
            expect_equal(length(result), 3)
            expect_named(result, c("x_1", "x_2", "x_3"))
          })

# Test for dataframe input with 2 elements per row
test_that("itvl_to_splx transformation works for dataframe input with 2 elements per row",
          {
            interval_bounds <- data.frame(rbind(c(0.1, 0.5), c(0.2, 0.6)))
            result <- itvl_to_splx(interval_bounds, min = 0, max = 1)
            expect_equal(ncol(result), 3)
            expect_equal(nrow(result), 2)
            expect_named(result, c("x_1", "x_2", "x_3"))
          })

# Test for invalid input
test_that("itvl_to_splx transformation handles invalid input", {
  expect_error(
    itvl_to_splx(c(0.4), min = 0, max = 1),
    "Interval bounds data must have 2 values per response\\!"
  )
  expect_error(
    itvl_to_splx(data.frame(rbind(c(
      0.1
    ), c(
      0.4
    ))), min = 0, max = 1),
    "Interval bounds data must have 2 values per response\\!"
  )
})


### Simplex to Interval ########################################################

# Test for vector input with 3 elements
test_that("splx_to_itvl transformation works for vector input with 3 elements",
          {
            simplex <- c(0.1, 0.5, 0.4)
            result <- splx_to_itvl(simplex, min = 0, max = 1)
            expect_equal(length(result), 2)
            expect_named(result, c("x_lo", "x_up"))
          })

# Test for dataframe input with 3 elements per row
test_that("splx_to_itvl transformation works for dataframe input with 3 elements per row",
          {
            simplex <- data.frame(rbind(c(0.1, 0.5, 0.4), c(0.2, 0.3, 0.5)))
            result <- splx_to_itvl(simplex, min = 0, max = 1)
            expect_equal(ncol(result), 2)
            expect_equal(nrow(result), 2)
            expect_named(result, c("x_lo", "x_up"))
          })

# Test for invalid input
test_that("splx_to_itvl transformation handles invalid input", {
  expect_error(splx_to_itvl(c(0.4, 0.2), min = 0, max = 1), "Simplex must have 3 elements")
  expect_error(splx_to_itvl(data.frame(rbind(
    c(0.1, 0.2), c(0.4, 0.5)
  )), min = 0, max = 1),
  "Simplex must have 3 elements")
})
