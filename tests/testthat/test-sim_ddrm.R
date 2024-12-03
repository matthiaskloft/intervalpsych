test_that("rowsums of responses are 1", {
  expect_equal(
    {
      n_items <- 10
      n_respondents <- 20
      x <- sim_ddrm(n_respondents = n_respondents, n_items = n_items)

      sum(x$x1, x$x2, x$x3)
    },
    n_respondents * n_items
  )
})
