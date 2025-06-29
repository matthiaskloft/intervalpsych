#' Custom ggplot2 Theme for intervalpsych
#'
#' This function creates a custom ggplot2 theme for intervalpsych visualizations.
#'
#' @param hide_axis_text_y Logical. If TRUE, the y-axis text and ticks will be hidden. Default is FALSE.
#' @param base_size Numeric. Base font size for the theme. Default is 12.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' # Create sample interval data
#' df_intervals <- data.frame(
#'   lower = c(0.1, 0.3, 0.2, 0.4),
#'   upper = c(0.5, 0.7, 0.6, 0.8)
#' )
#' item_labels <- c("Item A", "Item B", "Item C", "Item D")
#' 
#' # Basic usage
#' plot_intervals(df_intervals, item_labels) +
#'   theme_icm()
#' 
#' # Hide y-axis text
#' plot_intervals(df_intervals, item_labels) +
#'   theme_icm(hide_axis_text_y = TRUE)
#' 
#' # Custom base size
#' plot_intervals(df_intervals, item_labels) +
#'   theme_icm(base_size = 14)
#'
#' @import ggplot2
#' @export
#'
theme_icm <- function(hide_axis_text_y = FALSE,
                      base_size = 12) {
  #showtext_auto()
  # theme
  theme <-
    ggplot2::theme_minimal(base_family = "sans", base_size = base_size) +
    ggplot2::theme(
      # remove minor grid
      panel.grid.minor = ggplot2::element_blank(),
      # Title and Axis Texts
      plot.title = ggplot2::element_text(
        face = "plain",
        size = ggplot2::rel(1.25),
        hjust = 0.5
      ),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.3), hjust = 0.5),
      axis.text.x = ggplot2::element_text(face = "plain", size = ggplot2::rel(1.1)),
      axis.text.y = ggplot2::element_text(face = "plain", size = ggplot2::rel(1.1)),
      axis.title.x = ggplot2::element_text(face = "plain", size = ggplot2::rel(1.25)),
      axis.title.y = ggplot2::element_text(
        face = "plain",
        size = ggplot2::rel(1.25),
        vjust = .3
      ),
      axis.line = element_line(colour = "#6d6d6e"),

      # Faceting
      strip.text = ggplot2::element_text(
        face = "plain",
        size = ggplot2::rel(1.1),
        hjust = 0.5
      ),
      strip.text.x.top = ggplot2::element_text(
        face = "plain",
        size = ggplot2::rel(1.2),
        hjust = 0.5
      ),
      # strip.text.y = element_blank(),
      strip.background = ggplot2::element_rect(fill = NA, color = NA),
      # Grid
      panel.grid = ggplot2::element_line(colour = "#F3F4F5"),
      # Legend
      legend.title = ggplot2::element_text(face = "plain"),
      legend.position = "top",
      legend.justification = 1,
      # Panel/Facets
      panel.spacing.x = ggplot2::unit(1.6, "lines"),
      panel.spacing.y = ggplot2::unit(1.6, "lines"),
      # Remove vertical grid lines
      panel.grid.major.x = ggplot2::element_blank()

    )
  # hide y axis text
  if (isTRUE(hide_axis_text_y)) {
    theme <-
      theme +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  } # end if

  return(theme)
}
