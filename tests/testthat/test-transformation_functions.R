


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


### SLR ########################################################################

# Test for vector input
test_that("slr transformation works for vector input", {
  simplex <- c(0.4, 0.2, 0.4)
  result <- slr(simplex)
  expect_equal(length(result), 2)
  expect_named(result, c("x_loc", "x_wid"))
  expect_true(is.numeric(result))
})

# Test for dataframe input
test_that("slr transformation works for dataframe input", {
  simplex <- data.frame(rbind(c(0.1, 0.2, 0.7), c(0.4, 0.5, 0.1)))
  result <- slr(simplex)
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 2)
  expect_named(result, c("x_loc", "x_wid"))
  expect_true(is.data.frame(result))
})

# Test for matrix input
test_that("slr transformation works for matrix input", {
  simplex <- matrix(c(0.1, 0.2, 0.7, 0.4, 0.5, 0.1), nrow = 2, byrow = TRUE)
  result <- slr(simplex)
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 2)
  expect_named(result, c("x_loc", "x_wid"))
})

# Test for invalid input - wrong number of elements
test_that("slr transformation handles invalid input", {
  expect_error(slr(c(0.4, 0.2)), "Simplex must have 3 elements")
  expect_error(slr(data.frame(rbind(
    c(0.1, 0.2), c(0.4, 0.5)
  ))), "Simplex must have 3 elements")
})

# Test for invalid simplex - non-numeric
test_that("slr handles non-numeric input", {
  expect_error(slr(c("a", "b", "c")), "Error: vector must be numeric!")
})

# Test for invalid simplex - doesn't sum to 1
test_that("slr handles simplex that doesn't sum to 1", {
  expect_error(slr(c(0.4, 0.2, 0.5)), "Error: \\(row-\\)vector must sum to 1!")
})

# Test for invalid simplex - contains zeros
test_that("slr handles simplex with zero elements", {
  expect_error(slr(c(0.4, 0.6, 0)), "Error: None of the elements in the \\(row-\\)vector must be exactly 0! Please apply padding first!")
})

# Test specific values
test_that("slr produces expected values for known inputs", {
  # Test symmetric case (equal left and right parts)
  simplex <- c(1/3, 1/3, 1/3)
  result <- slr(simplex)
  expect_equal(unname(result[1]), 0, tolerance = 1e-10)  # x_loc should be 0 for symmetric case
  expect_equal(unname(result[2]), -log(2), tolerance = 1e-10)  # x_wid should be -log(2)
  
  # Test case where middle component is very small
  simplex <- c(0.4, 0.0001, 0.5999)  # Very small middle component
  result <- slr(simplex)
  expect_true(is.finite(result[1]))
  expect_true(is.finite(result[2]))
})


### Inverse SLR ################################################################

# Test for vector input
test_that("inv_slr transformation works for vector input", {
  bvn <- c(0, 0.2)
  result <- inv_slr(bvn)
  expect_equal(length(result), 3)
  expect_named(result, c("x_1", "x_2", "x_3"))
  expect_true(is.numeric(result))
  expect_equal(sum(result), 1, tolerance = 1e-10)  # Should sum to 1
})

# Test for dataframe input
test_that("inv_slr transformation works for dataframe input", {
  bvn <- data.frame(rbind(c(0, 0.2), c(-2, 0.4)))
  result <- inv_slr(bvn)
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 2)
  expect_named(result, c("x_1", "x_2", "x_3"))
  expect_true(is.data.frame(result))
  # Each row should sum to 1
  expect_equal(rowSums(result), c(1, 1), tolerance = 1e-10)
})

# Test for matrix input
test_that("inv_slr transformation works for matrix input", {
  bvn <- matrix(c(0, 0.2, -2, 0.4), nrow = 2, byrow = TRUE)
  result <- inv_slr(bvn)
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 2)
  expect_named(result, c("x_1", "x_2", "x_3"))
  # Each row should sum to 1
  expect_equal(rowSums(result), c(1, 1), tolerance = 1e-10)
})

# Test for invalid input - wrong number of elements
test_that("inv_slr transformation handles invalid input", {
  expect_error(inv_slr(c(0.4)),
               "Error: \\(row-\\)vector must have exactly 2 elements\\!")
  expect_error(inv_slr(data.frame(rbind(c(
    0.1
  ), c(
    0.4
  )))),
  "Error: \\(row-\\)vector must have exactly 2 elements\\!")
})

# Test for invalid input - non-numeric
test_that("inv_slr handles non-numeric input", {
  expect_error(inv_slr(c("a", "b")), "Error: vector must be numeric!")
})

# Test specific values
test_that("inv_slr produces expected values for known inputs", {
  # Test zero input should give symmetric result
  bvn <- c(0, 0)
  result <- inv_slr(bvn)
  expect_true(all(result > 0))  # All components should be positive
  expect_equal(sum(result), 1, tolerance = 1e-10)
  
  # Test that all components are within [0,1]
  bvn <- c(1.5, -0.5)
  result <- inv_slr(bvn)
  expect_true(all(result >= 0))
  expect_true(all(result <= 1))
  expect_equal(sum(result), 1, tolerance = 1e-10)
})


### Roundtrip Tests ############################################################

# Test that slr and inv_slr are inverse functions
test_that("slr and inv_slr are inverse transformations for vectors", {
  # Test with various simplex values
  test_simplices <- list(
    c(0.1, 0.2, 0.7),
    c(0.4, 0.5, 0.1),
    c(1/3, 1/3, 1/3),
    c(0.2, 0.3, 0.5)
  )
  
  for (simplex in test_simplices) {
    # Forward and back
    slr_result <- slr(simplex)
    roundtrip <- inv_slr(slr_result)
    expect_equal(unname(roundtrip), unname(simplex), tolerance = 1e-10)
  }
})

test_that("slr and inv_slr are inverse transformations for dataframes", {
  simplex_df <- data.frame(rbind(
    c(0.1, 0.2, 0.7),
    c(0.4, 0.5, 0.1),
    c(0.2, 0.3, 0.5)
  ))
  
  # Forward and back
  slr_result <- slr(simplex_df)
  roundtrip <- inv_slr(slr_result)
  expect_equal(unname(as.matrix(roundtrip)), unname(as.matrix(simplex_df)), tolerance = 1e-10)
})

test_that("inv_slr and slr are inverse transformations", {
  # Test with various bvn values
  test_bvns <- list(
    c(0, 0.2),
    c(-2, 0.4),
    c(1.5, -0.5),
    c(0, 0)
  )
  
  for (bvn in test_bvns) {
    # Forward and back
    inv_slr_result <- inv_slr(bvn)
    roundtrip <- slr(inv_slr_result)
    expect_equal(unname(roundtrip), unname(bvn), tolerance = 1e-10)
  }
})

# Test that SLR preserves simplex constraints
test_that("inv_slr always produces valid simplices", {
  # Test with extreme values
  test_bvns <- list(
    c(-5, 5),
    c(5, -5),
    c(0, 10),
    c(-10, 0)
  )
  
  for (bvn in test_bvns) {
    result <- inv_slr(bvn)
    expect_true(all(result >= 0), info = paste("Negative values for bvn:", paste(bvn, collapse = ", ")))
    expect_true(all(result <= 1), info = paste("Values > 1 for bvn:", paste(bvn, collapse = ", ")))
    expect_equal(sum(result), 1, tolerance = 1e-10, info = paste("Sum != 1 for bvn:", paste(bvn, collapse = ", ")))
  }
})
