test_that("plot_consensus works correctly with method = 'median_bounds'", {
  suppressWarnings(
    fit <-
      intervalpsych::fit_itm(
        df_simplex = extraDistr::rdirichlet(25, c(2, 2, 2)) |> as.data.frame(),
        id_person = rep(1:5, 5),
        id_item = rep(1:5, each = 5),
        n_chains = 1,
        n_cores = 1,
        iter_sampling = 50,
        iter_warmup = 50,
        adapt_delta = .8
      )
  )

  plot <- intervalpsych::plot_consensus(fit, method = "median_bounds")

  expect_s3_class(plot, "ggplot")
})


test_that("plot_consensus works correctly with method = 'draws_distribution'",
          {
            suppressWarnings(
              fit <-
                intervalpsych::fit_itm(
                  df_simplex = extraDistr::rdirichlet(25, c(2, 2, 2)) |> as.data.frame(),
                  id_person = rep(1:5, 5),
                  id_item = rep(1:5, each = 5),
                  n_chains = 1,
                  n_cores = 1,
                  iter_sampling = 50,
                  iter_warmup = 50,
                  adapt_delta = .8
                )
            )
            suppressWarnings(plot <- intervalpsych::plot_consensus(fit, method = "draws_distribution"))

            expect_s3_class(plot, "ggplot")
            expect_warning(
              intervalpsych::plot_consensus(fit, method = "draws_distribution"),
              "Number of draws is less than 1000. Consider increasing the number of iterations or using method = 'median_bounds' instead."
            )

          })

test_that("plot_consensus throws errors", {
  fit <-  list()
  expect_error(
    intervalpsych::plot_consensus(fit, method = "median_bounds"),
    "Input must be an object of class 'itm_stanfit'"
  )

})
