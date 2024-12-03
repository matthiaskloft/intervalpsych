test_that("ILR: equal components result in zero location and width", {
  expect_equal(ilr(c(1 / 3, 1 / 3, 1 / 3)), c(x_loc = 0, x_wid = 0))
})

test_that("Inverse ILR: zero location and width result in equally spaced composition", {
  expect_equal(inv_ilr(c(0, 0)), c(x_1 = 1 / 3, x_2 = 1 / 3, x_3 = 1 / 3))
})
