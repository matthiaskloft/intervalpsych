# Helper function to generate Dirichlet samples using rgamma
generate_dirichlet <- function(n, alpha) {
  k <- length(alpha)
  samples <- matrix(0, n, k)
  for(i in 1:n) {
    x <- rgamma(k, alpha, 1)
    samples[i, ] <- x / sum(x)
  }
  samples
}

test_that("extract_consensus works correctly", {

suppressWarnings(
fit <-
  intervalpsych::fit_icm(
    df_simplex = generate_dirichlet(25, c(2,2,2)) |> as.data.frame(),
    id_person = rep(1:5,5),
    id_item = rep(1:5, each = 5),
    n_chains = 1,
    n_cores = 1,
    iter_sampling = 50,
    iter_warmup = 50,
    adapt_delta = .8
  )
)
  consensus <- intervalpsych::extract_consensus(fit)

  expect_s3_class(consensus$summary, "data.frame")
  expect_s3_class(consensus$df_rvar, "data.frame")

  expect_true(all(c("df_rvar", "summary") %in% names(consensus)))

  expect_true(nrow(consensus$summary) > 0)
  expect_true(all(c("T_L_median",
                    "T_L_CI_025",
                    "T_L_CI_975",
                    "T_U_median",
                    "T_U_CI_025",
                    "T_U_CI_975"
                    ) %in% colnames(consensus$summary)))

})
