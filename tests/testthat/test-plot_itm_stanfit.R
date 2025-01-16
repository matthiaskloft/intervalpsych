test_that("plot.itm_stanfit works correctly", {

  suppressWarnings(
    fit <-
      intervalpsych::fit_itm(
        df_simplex = extraDistr::rdirichlet(25, c(2,2,2)) |> as.data.frame(),
        id_person = rep(1:5,5),
        id_item = rep(1:5, each = 5),
        n_chains = 1,
        n_cores = 1,
        iter_sampling = 50,
        iter_warmup = 50,
        adapt_delta = .8
      )
  )

  plot <- plot(fit)

  expect_s3_class(plot, "ggplot")

})
