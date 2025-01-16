test_that("extract_consensus works correctly", {
# data(quantifiers)
#
# quantifiers <- quantifiers |>
#   # exclude control items
#     dplyr::filter(name_en %in% c("seldom", "often", "almost always", "almost never", "frequently")) |>
#   # sample 100 respondents
#   dplyr::filter(id_person %in% sample(
#     size = 5,
#     replace = FALSE,
#     unique(quantifiers$id_person)
#   )) |>
#   # exclude missing values
#   dplyr::filter(!is.na(x_L) & !is.na(x_U)) |>
#   # recompute IDs
#   dplyr::mutate(
#     id_person = factor(id_person) |> as.numeric(),
#     id_item = factor(id_item) |> as.numeric()
#   )
#
# quantifiers <- cbind(
#   quantifiers,
#   intervalpsych::itvl_to_splx(quantifiers[,c("x_L","x_U")], min = quantifiers$scale_min, max = quantifiers$scale_max))
#
# quantifiers[, c("x_1", "x_2", "x_3")] <-
#   intervalpsych::remove_zeros(quantifiers[, c("x_1", "x_2", "x_3")], padding = 0.01)
#
# fit <-
#   intervalpsych::fit_itm(
#     df_simplex = quantifiers[, c("x_1", "x_2", "x_3")],
#     id_person = quantifiers$id_person,
#     id_item = quantifiers$id_item,
#     item_labels = quantifiers |>
#       dplyr::distinct(id_item, name_en) |>
#       dplyr::pull(name_en),
#     n_chains = 1,
#     n_cores = 1,
#     iter_sampling = 50,
#     iter_warmup = 50,
#     adapt_delta = .8
#   )

fit <-
  intervalpsych::fit_itm(
    df_simplex = stats::rdir(50) |> as.data.frame(),
    id_person = rep(1:10,5),
    id_item = rep(1:5, each = 5),
    item_labels = quantifiers |>
      dplyr::distinct(id_item, name_en) |>
      dplyr::pull(name_en),
    n_chains = 1,
    n_cores = 1,
    iter_sampling = 50,
    iter_warmup = 50,
    adapt_delta = .8
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
