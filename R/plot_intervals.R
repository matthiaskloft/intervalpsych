#' Plot Intervals
#'
#' Plot intervals from a data frame of interval bounds.
#'
#' @param df_interval_bounds A data frame with two columns: the lower and upper
#' bounds of the intervals.
#'
#' @param item_labels An optional vector of labels for the items. Its length
#' must match the number of rows in `df_interval_bounds`.
#'
#' @return A ggplot object depicting the intervals.
#'
#' @import ggplot2
#'
#' @examples
#' df <- data.frame(lower = c(0.1, 0.3, 0.5), upper = c(0.4, 0.6, 0.8))
#' labels <- c("Item 1", "Item 2", "Item 3")
#'
#' plot_intervals(df, item_labels = labels)
#'
#' @export
plot_intervals <- function(df_interval_bounds, item_labels = NULL) {
  ### checks -------------------------------------------------------------------

  # check length of item_labels
  if (!is.null(item_labels)) {
    if (length(item_labels) != nrow(df_interval_bounds)) {
      stop("Length of item_labels must match number of rows in df_interval_bounds")
    }
  }

  ### plot ---------------------------------------------------------------------

  # prepare data for plotting
  df_plot <- df_interval_bounds
  names(df_plot) <- c("lower", "upper")

  # if item_labels is provided, use it
  if (!is.null(item_labels)) {
    df_plot$item <- factor(item_labels)
  } else {
    df_plot$item <- factor(seq_len(nrow(df_interval_bounds)))
  }

  # plot
  plot <-
    df_plot |>
    ggplot2::ggplot() +
    ggplot2::geom_errorbarh(
      ggplot2::aes(
        y = .data$item,
        xmin = .data$lower,
        xmax = .data$upper
      ),
      height = .5,
      linewidth = .5
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, .25),
      labels = c("0", ".25", ".50", ".75", "1"),
      expand = ggplot2::expansion(0, 0)
    ) +
    ggplot2::labs(x = "Interval Response", y = "Item") +
    theme_icm(base_size = 12, hide_axis_text_y = FALSE)

  return(plot)

}
