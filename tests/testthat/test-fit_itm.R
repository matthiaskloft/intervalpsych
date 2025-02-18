test_that("fit_itm works correctly", {

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

  expect_s3_class(fit, "list")
  expect_s3_class(fit, "icm_stanfit")

  expect_true(all(c("stan_model" ,"stan_fit", "stan_data", "item_labels") %in% names(fit)))

})


test_that("fit_itm function checks the input data correctly", {

  df_simplex <- data.frame(matrix(runif(30), nrow=10, ncol=3))
  id_person <- rep(1:5, each=2)
  id_item <- rep(1:2, times=5)

  # Test for error when df_simplex is not a dataframe
  expect_error(fit_itm(matrix(runif(30), nrow=10, ncol=3), id_person, id_item),
               "Error: simplex must be a dataframe!")

  # Test for error when id_person length does not match number of rows in df_simplex
  expect_error(fit_itm(df_simplex, rep(1:4, each=2), id_item),
               "Error: id_person must have the same length as the number of rows in the simplex!")

  # Test for error when id_item length does not match number of rows in df_simplex
  expect_error(fit_itm(df_simplex, id_person, rep(1:3, each=3)),
               "Error: id_item must have the same length as the number of rows in the simplex!")

  # Test for error when item_labels length does not match number of rows in df_simplex or unique id_item
  expect_error(fit_itm(df_simplex, id_person, id_item, item_labels = rep("A", 5)),
               "Error: item_labels must have the same length as the number of rows in the simplex or the number of unique elements in id_item!")

  # Test for error when id_person contains non-natural numbers
  expect_error(fit_itm(df_simplex, c(1.5, 2, 3, 4, 5, 1, 2, 3, 4, 5), id_item),
               "Error: id_person must be natural numbers!")

  # Test for error when id_item contains non-natural numbers
  expect_error(fit_itm(df_simplex, id_person, c(1, 2, 3, 4, 5, 1.5, 2, 3, 4, 5)),
               "Error: id_item must be natural numbers!")

  # Test for error when df_simplex contains NAs
  df_simplex_na <- df_simplex
  df_simplex_na[1, 1] <- NA
  expect_error(fit_itm(df_simplex_na, id_person, id_item),
               "Error: simplex contains NAs!")

  # Test for error when df_simplex does not have 3 columns
  expect_error(fit_itm(data.frame(matrix(runif(40), nrow=10, ncol=4)), id_person, id_item),
               "Simplex must have 3 elements")
})
