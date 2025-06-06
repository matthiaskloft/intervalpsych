test_that("gather_values works correctly with weighted = TRUE", {
  lower <- c(1, 2, 3)
  upper <- c(2, 3, 4)
  cluster_id <- c(1, 2, 3)
  weighted <- TRUE
  n_samples <- 100

  result <- gather_values(lower, upper, cluster_id, weighted, n_samples = n_samples)

  expect_equal(nrow(result), length(lower) * n_samples)
  expect_equal(ncol(result), 2)
  expect_true(all(result$cluster_id %in% cluster_id))
  expect_true(all(result$samples >= lower[1] & result$samples <= upper[3]))
})

test_that("gather_values works correctly with weighted = FALSE", {
  lower <- c(1, 2, 3)
  upper <- c(2, 3, 4)
  cluster_id <- c(1, 2, 3)
  weighted <- FALSE
  step_size <- 0.1

  result <- gather_values(lower, upper, cluster_id, weighted, step_size = step_size)

  expect_true(nrow(result) > length(lower))
  expect_equal(ncol(result), 2)
  expect_true(all(result$cluster_id %in% cluster_id))
  expect_true(all(result$samples >= lower[1] & result$samples <= upper[3]))
})

test_that("ggplot_cumulative_intervals works correctly", {
  data <- data.frame(samples = runif(100, 1, 10), cluster_id = rep(1:2, each = 50))
  min <- 1
  max <- 10
  binwidth <- 0.5

  plot <- ggplot_cumulative_intervals(data, min, max, binwidth)

  expect_s3_class(plot, "ggplot")
})


test_that("ggplot_cumulative_intervals stops if min or max is NULL", {
  data <- data.frame(samples = runif(100, 1, 10), cluster_id = rep(1:2, each = 50))
  binwidth <- 0.5

  expect_error(ggplot_cumulative_intervals(data, NULL, 10, binwidth), "min and max must be specified")
  expect_error(ggplot_cumulative_intervals(data, 1, NULL, binwidth), "min and max must be specified")
})

test_that("ggplot_cumulative_intervals stops if binwidth is NULL", {
  data <- data.frame(samples = runif(100, 1, 10), cluster_id = rep(1:2, each = 50))
  min <- 1
  max <- 10

  expect_error(ggplot_cumulative_intervals(data, min, max, NULL), "binwidth must be specified")
})


test_that("plot_intervals_cumulative works correctly", {
  lower <- c(1, 2, 3)
  upper <- c(2, 3, 4)
  cluster_id <- c(1, 2, 3)
  truth <- c(1.5, 2.5, 3.5)
  min_val <- 0
  max_val <- 5
  facet_wrap <- TRUE
  weighted <- TRUE
  show_quantiles <- TRUE
  ncol <- 2

  plot <- plot_intervals_cumulative(
    lower,
    upper,
    cluster_id,
    truth,
    min_val,
    max_val,
    facet_wrap,
    weighted,
    show_quantiles,
    ncol
  )

  expect_s3_class(plot, "ggplot")
  expect_true("truth" %in% names(plot$data))
  expect_true("samples" %in% names(plot$data))
  expect_true("cluster_id" %in% names(plot$data))
  expect_true("median" %in% names(plot$data))
  expect_true("q_05" %in% names(plot$data))
  expect_true("q_95" %in% names(plot$data))
})
