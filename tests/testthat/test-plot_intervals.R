test_that("plot_intervals works with basic input", {
  df <- data.frame(
    lower = c(0.1, 0.3, 0.5),
    upper = c(0.4, 0.6, 0.8)
  )

  plot <- intervalpsych::plot_intervals(df)

  expect_s3_class(plot, "ggplot")
})

test_that("plot_intervals works with item labels", {
  df <- data.frame(
    lower = c(0.1, 0.3, 0.5),
    upper = c(0.4, 0.6, 0.8)
  )

  labels <- c("Item 1", "Item 2", "Item 3")

  plot <- intervalpsych::plot_intervals(df, item_labels = labels)

  expect_s3_class(plot, "ggplot")
})

test_that("plot_intervals throws error for mismatched item_labels length", {
  df <- data.frame(
    lower = c(0.1, 0.3, 0.5),
    upper = c(0.4, 0.6, 0.8)
  )

  labels <- c("Item 1", "Item 2")  # Wrong length

  expect_error(
    intervalpsych::plot_intervals(df, item_labels = labels),
    "Length of item_labels must match number of rows in df_interval_bounds"
  )
})

test_that("plot_intervals works with single interval", {
  df <- data.frame(
    lower = 0.2,
    upper = 0.7
  )

  plot <- intervalpsych::plot_intervals(df)

  expect_s3_class(plot, "ggplot")
})

test_that("plot_intervals works with edge case values", {
  df <- data.frame(
    lower = c(0, 0.5),
    upper = c(0.5, 1)
  )

  plot <- intervalpsych::plot_intervals(df)

  expect_s3_class(plot, "ggplot")
})
