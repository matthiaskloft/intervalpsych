
#' @title Plot aggregated interval responses
#'
#' @param data A data.frame with columns in the specific order of (1) IDs
#' (only if multiple is set to TRUE), (2) lower bounds of interval responses,
#' and (3) upper bounds of interval responses. All responses must be scaled to
#' lie in (0,1).
#'
#' @param multiple Do the data contain responses for multiple respondents?
#' If TRUE, IDs must be supplied in the first column.
#'
#' @param step_size Resolution of the x-axis.
#'
#' @param show_quantiles Show median, .025-percentile, and .975-percentile as
#' vertical lines.
#'
#'
#' @return A ggplot2 object containing a plot of the aggregated interval responses.
#'
#' @export
#'
#' @examples
#' # simulate responses
#' x <- sim_ddrm(n_respondents = 1, n_items = 20)[,3:5]
#' # convert to interval format
#' x <- splx_to_itvl(x, min = 0, max = 1)
#' # plot the aggregated response intervals
#' plot_intvls_aggregated(x)
#'
#'
plot_intvls_aggregated <- function(data,
                                   multiple = FALSE,
                                   step_size = .01,
                                   show_quantiles = TRUE) {
  # gather values between bounds
  gathered <- gather_values(data = data,
                            step_size = step_size)
  # plot
  plot <-
    ggplot_aggregated_single(data = gathered,
                             step_size = step_size,
                             show_quantiles = show_quantiles)

  return(plot)
}

# helper: gather interval values -----------------------------------------------
gather_values <- function(data, step_size = step_size) {
  # identify columns for lower and upper response value
  idx_lo <- ncol(data) - 1
  idx_up <- ncol(data)

  df_gathered <-
    Y <- apply(
      X = data,
      MARGIN = 1,
      FUN = function(X) {
        # gather values between bounds by step size
        return(seq(X[idx_lo], X[idx_up], step_size))
      }
    )
  # format as single column df
  Y <- as.numeric(unlist(Y))
  out <- data.frame(value = Y)

  return(out)
}

# helper: ggplot for aggregated responses --------------------------------------
ggplot_aggregated_single <-
  function(data,
           step_size = step_size,
           show_quantiles = TRUE) {

    # avoid warnings
    value <- NULL

    # plot
    plot <-
      ggplot2::ggplot(data, ggplot2::aes(x = value)) +
      ggplot2::geom_area(
        stat = "bin",
        color = "black",
        fill = "gray90",
        binwidth = step_size,
        linewidth = .7
      ) +
      ggplot2::scale_x_continuous(
        limits = c(0, 1),
        breaks = c(0, .25, .5, .75, 1),
        labels = c("0", ".25", ".50", ".75", "1"),
        expand = ggplot2::expansion()
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = c(0, .5))) +
      ggplot2::labs(x = "Aggregated Response Intervals",
                    y = "Density") +
      ggplot2::theme_bw(base_family = "serif", base_size = 12) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect()
      )

    if (show_quantiles == TRUE) {
      # add quantiles to the plot
      plot <- plot +
        ggplot2::geom_vline(
          ggplot2::aes(xintercept =  stats::median(x = value, na.rm = TRUE)),
          col = "black",
          linetype = 1,
          linewidth = .6
        ) +
        ggplot2::geom_vline(
          ggplot2::aes(xintercept = stats::quantile(x = value, .025, na.rm = TRUE)),
          col = "black",
          linetype = 2,
          linewidth = .7
        ) +
        ggplot2::geom_vline(
          ggplot2::aes(xintercept = stats::quantile(x = value, .975, na.rm = TRUE)),
          col = "black",
          linetype = 2,
          linewidth = .7
        )
    }
    return(plot)
  }

# helper: ggplot for aggregated responses - multiple respondents ---------------
ggplot_aggregated_multiple <-
  function(data,
           step_size = .01,
           show_quantiles = TRUE) {

    # avoid warnings
    value <- NULL

    # plot
    plot <-
      ggplot2::ggplot(data, ggplot2::aes(x = value)) +
      ggplot2::geom_area(
        stat = "bin",
        color = "black",
        fill = "gray90",
        binwidth = step_size,
        linewidth = .7
      ) +
      ggplot2::scale_x_continuous(
        limits = c(0, 1),
        breaks = c(0, .25, .5, .75, 1),
        labels = c("0", ".25", ".50", ".75", "1"),
        expand = ggplot2::expansion()
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = c(0, .5))) +
      ggplot2::labs(x = "Aggregated Response Intervals",
                    y = "Density") +
      ggplot2::theme_bw(base_family = "serif", base_size = 12) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect()
      )

    if (show_quantiles == TRUE) {
      # add quantiles to the plot
      plot <- plot +
        ggplot2::geom_vline(
          ggplot2::aes(xintercept =  stats::median(x = value, na.rm = TRUE)),
          col = "black",
          linetype = "dashed"
        ) +
        ggplot2::geom_vline(
          ggplot2::aes(xintercept = stats::quantile(x = value, .025, na.rm = TRUE)),
          col = "black",
          linetype = "dotted"
        ) +
        ggplot2::geom_vline(
          ggplot2::aes(xintercept = stats::quantile(x = value, .975, na.rm = TRUE)),
          col = "black",
          linetype = "dotted"
        )
    }
    return(plot)
  }





# n <- 25
# idx <- sample(1:max(rs2_long$ii), size = n, replace = FALSE)
# integral_df <- map_dfr(idx, function(idx) {
#   data.frame(ii = idx,
#              x = integral_list[[idx]] %>% as.double())
# })
# x_stats <-
#   integral_df %>% group_by(ii) %>% mutate(median = median(x),
#                                           q025 = quantile(x, .025),
#                                           q975 = quantile(x, .975)) %>%
#   ungroup() %>%
#   distinct(ii, .keep_all = TRUE)

# ggplot_aggregated_multiple <- function(data){
#
#     ggplot(data, aes(x = x, group = ii)) +
#     geom_freqpoly(
#       binwidth = 1,
#       col = "black",
#       size = .7
#     ) +
#     geom_vline(aes(xintercept = median), x_stats, col = "red") +
#     geom_vline(aes(xintercept = q025), x_stats, col = "blue") +
#     geom_vline(aes(xintercept = q975), x_stats, col = "blue") +
#     facet_wrap( ~ ii, ncol = 5) +
#     labs(x = "Cumulative Response Intervals",
#          y = "Density") +
#     theme_bw(base_family = "serif", base_size = 12) +
#     theme(panel.grid = element_blank(),
#           strip.background =element_rect())
#
# }



