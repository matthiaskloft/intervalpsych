test_that("theme_icm returns a ggplot2 theme object", {
  result <- intervalpsych::theme_icm()
  expect_s3_class(result, "theme")
})

test_that("theme_icm hides y-axis text and ticks when hide_axis_text_y is TRUE", {
  result <- intervalpsych::theme_icm(hide_axis_text_y = TRUE)
  expect_equal(result$axis.text.y, ggplot2::element_blank())
  expect_equal(result$axis.ticks.y, ggplot2::element_blank())
})

test_that("theme_icm sets base font size correctly", {
  base_size <- 14
  result <- intervalpsych::theme_icm(base_size = base_size)
  expect_equal(result$text$size, base_size)
})

test_that("theme_icm sets plot title size correctly", {
  result <- intervalpsych::theme_icm()
  expect_equal(result$plot.title$size,ggplot2::rel(1.25))
})

test_that("theme_icm sets axis text size correctly", {
  result <- intervalpsych::theme_icm()
  expect_equal(result$axis.text.x$size, ggplot2::rel(1.1))
  expect_equal(result$axis.text.y$size, ggplot2::rel(1.1))
})

test_that("theme_icm sets axis title size correctly", {
  result <- intervalpsych::theme_icm()
  expect_equal(result$axis.title.x$size, ggplot2::rel(1.25))
  expect_equal(result$axis.title.y$size, ggplot2::rel(1.25))
})

test_that("theme_icm sets legend position correctly", {
  result <- intervalpsych::theme_icm()
  expect_equal(result$legend.position, "top")
  expect_equal(result$legend.justification, 1)
})
