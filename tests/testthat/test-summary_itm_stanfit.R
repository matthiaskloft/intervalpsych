test_that("summary.itm_stanfit works correctly", {

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

  summary <- summary(fit)

  expect_s3_class(summary, "data.frame")

  expect_true(nrow(summary) > 0)
  expect_true(ncol(summary) > 0)


  expect_true(all(c("T_L_median",
                    "T_L_CI_025",
                    "T_L_CI_975",
                    "T_U_median",
                    "T_U_CI_025",
                    "T_U_CI_975"
  ) %in% colnames(summary)))

})
