#' Gather Values from Intervals
#'
#' This function gathers values from specified intervals, either weighted by interval width or not.
#'
#' @param lower A numeric vector of lower bounds of the intervals.
#' @param upper A numeric vector of upper bounds of the intervals.
#' @param cluster_id An optional vector of cluster IDs. If not provided, a vector of ones will be created.
#' @param weighted A logical value indicating whether the values should be weighted by interval width.
#' @param step_size A numeric value specifying the step size when `weighted` is FALSE.
#' @param n_samples A numeric value specifying the number of samples when `weighted` is TRUE.
#'
#' @return A data frame with columns `samples` and `cluster_id`.
#'
#' @details
#' - If `weighted` is TRUE, values are sampled uniformly within each interval.
#' - If `weighted` is FALSE, values are gathered at regular intervals specified by `step_size`.
#'
#' @importFrom purrr map_dfr
#' @importFrom stats runif
#'
#' @noRd
#'
gather_values <- function(lower,
                          upper,
                          cluster_id = NULL,
                          weighted = NULL,
                          step_size = NULL,
                          n_samples = NULL) {
  # check that lower and upper are the same length
  if (length(lower) != length(upper)) {
    stop("lower and upper must be the same length")
  }

  # check that lower and upper are numeric
  if (!is.numeric(lower) | !is.numeric(upper)) {
    stop("lower and upper must be numeric")
  }

  # check tha lower is smaller than upper or to equal
  if (any(lower > upper)) {
    stop("lower must NOT be greater than upper")
  }

  # check that weighted is not NULL
  if (is.null(weighted)) {
    stop("weighted must be specified")
  }

  # if cluster_id is NOT NULL, check that it is the same length as lower and upper
  if (!is.null(cluster_id)) {
    if (length(cluster_id) != length(lower)) {
      stop("cluster_id must be the same length as lower and upper")
    }
  }

  # if cluster_id is NULL, create a vector of ones
  if (is.null(cluster_id)) {
    cluster_id <- rep(1, length(lower))
  }

  # weighted = TRUE -----------------------------------------------------------
  # values are weighted by interval width
  if (weighted) {
    # check that n_samples is not NULL
    if (is.null(n_samples)) {
      stop("n_samples must be specified when weighted = TRUE")
    }

    # map over intervals
    df_out <- purrr::map_dfr(1:length(lower), function(.x) {
      # if lower is equal to upper, return lower
      if (lower[.x] == upper[.x]) {
        samples <- rep(lower[.x], n_samples)

      } else {
        samples <- stats::runif(n = n_samples,
                                min = lower[.x],
                                max = upper[.x])
      }

      id <- rep(cluster_id[.x], length(samples))

      return(data.frame(samples = samples, cluster_id = id))
    })
  }

  # weighted = FALSE ---------------------------------------------------------
  # values are NOT weighted by interval width
  else {
    # check that step_size is not NULL
    if (is.null(step_size)) {
      stop("step_size must be specified when weighted = FALSE")
    }

    # map over intervals
    df_out <- purrr::map_dfr(1:length(lower), function(.x) {
      # gather values between bounds by step size
      samples <- seq(lower[.x], upper[.x], by = step_size) |> as.double()
      id <- rep(cluster_id[.x], length(samples))

      return(data.frame(samples = samples, cluster_id = id))
    })

  }

  return(df_out)
}


#' @title Plot Cumulative Intervals
#' @description This function creates a cumulative interval plot using ggplot2.
#' @param data A data frame containing the data to be plotted.
#' @param min The minimum value for the x-axis.
#' @param max The maximum value for the x-axis.
#' @param binwidth The width of the bins for the histogram.
#' @param facet_wrap A logical value indicating whether to use facet wrapping.
#' @param show_quantiles A logical value indicating whether to show quantiles on the plot.
#' @param ncol The number of columns for facet wrapping if design matrix is not provided.
#' @return A ggplot2 object representing the cumulative interval plot.
#'
#' @import ggokabeito
#' @import ggplot2
#' @importFrom ggdist stat_slab
#'
#' @noRd
#'
ggplot_cumulative_intervals <-
  function(data,
           min = NULL,
           max = NULL,
           binwidth = NULL,
           facet_wrap = FALSE,
           show_quantiles = TRUE,
           ncol = 2) {
    # check that min and max are not NULL
    if (is.null(min) | is.null(max)) {
      stop("min and max must be specified")
    }

    # check that binwidth is not NULL
    if (is.null(binwidth)) {
      stop("binwidth must be specified")
    }

    scale_min <- min(min, na.rm = TRUE)
    scale_max <- max(max, na.rm = TRUE)
    # plot ---------------------------------------------------------------------
    plot <-
      ggplot2::ggplot(data) +
      ggdist::stat_slab(
        aes(.data$samples),
        density = "bounded",
        fill = "gray95",
        color = "black",
        alpha = 0.5
      ) +
      ggplot2::geom_vline(
        aes(xintercept = .data$truth),
        color = ggokabeito::palette_okabe_ito(order = 1),
        linewidth = 1
      ) +
      # ggplot2::geom_errorbarh(
      #   ggplot2::aes(
      #     xmin = lower_mean_logit,
      #     xmax = upper_mean_logit,
      #     y = max_density * 1.15,
      #     height = max_density * .05
      #   ),
      #   col = "grey70",
      #   height = 0,
      #   linetype = 1,
      #   linewidth = 8
      # ) +
      # ggplot2::geom_errorbarh(
      #   ggplot2::aes(
      #     xmin = lower_mean,
      #     xmax = upper_mean,
      #     y = max_density * 1.15,
      #     height = max_density * .1
      #   ),
      #   col = "black",
      #   linetype = 1,
      #   linewidth = 1.5
      # ) +
      ggplot2::scale_x_continuous(limits = c(scale_min, scale_max),
                                  expand = ggplot2::expansion()) +
      ggplot2::scale_y_continuous(
        labels = c(c("0", ".25", ".50", ".75", "1")),
        breaks = seq(0, 1, .25),
        expand = ggplot2::expansion(mult = c(0, .01))
      ) +
      ggplot2::labs(x = "Response Value", y = "Density") +
      theme_icm() +
      theme(
        plot.margin = margin(.2, .5, .2, .2, "cm"),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "#6d6d6e", linewidth = .3),
        axis.ticks = element_line(colour = "#6d6d6e", linewidth = .3),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 11, margin = margin(0, 0, .3, 0, "cm"))
      )

    # add facet wrap ---------------------------------------------------------

    if (facet_wrap) {
      plot <- plot + ggplot2::facet_wrap( ~ cluster_id, scales = "free", ncol = ncol)
    }

    # add quantiles to the plot ------------------------------------------------
    if (show_quantiles) {
      plot <- plot +
        ggplot2::geom_vline(
          ggplot2::aes(xintercept = .data$median),
          col = "black",
          linetype = 1,
          linewidth = .6
        ) +
        ggplot2::geom_vline(
          ggplot2::aes(xintercept = .data$q_05),
          col = "black",
          linetype = 2,
          linewidth = .7
        ) +
        ggplot2::geom_vline(
          ggplot2::aes(xintercept = .data$q_95),
          col = "black",
          linetype = 2,
          linewidth = .7
        )
    }

    return(plot)
  }


#' Plot Cumulative Intervals
#'
#' This function generates a cumulative interval plot based on the provided lower and upper bounds, cluster IDs, and other optional parameters.
#'
#' @param lower A numeric vector of lower bounds.
#' @param upper A numeric vector of upper bounds.
#' @param cluster_id A vector of cluster IDs corresponding to the intervals.
#' @param truth A numeric vector of ground truth values. Default is NA.
#' @param min The minimum value for the x-axis.
#' @param max The maximum value for the x-axis.
#' @param facet_wrap A logical value indicating whether to use facet wrapping. Default is FALSE.
#' @param weighted An optional vector of weights for the intervals.
#' @param show_quantiles A logical value indicating whether to show quantiles on the plot. Default is TRUE.
#' @param ncol The number of columns for facet wrapping. Default is 2.
#'
#' @return A ggplot object representing the cumulative interval plot.
#'
#' @importFrom stats median
#' @importFrom stats quantile
#'
#' @export
#'
plot_intervals_cumulative <- function(lower,
                                      upper,
                                      cluster_id,
                                      truth = NA,
                                      min,
                                      max,
                                      facet_wrap = NULL,
                                      weighted = NULL,
                                      show_quantiles = TRUE,
                                      ncol = 3) {
  step_size <- min(max - min) / 1e3
  n_samples <- 1e3

  # if facet_wrap is NULL, use facet wrap option for multiple clusters
  if (is.null(facet_wrap)) {
    if (length(unique(cluster_id)) > 1) {
      facet_wrap <- TRUE
    } else {
      facet_wrap <- FALSE
    }
  }

  # gather values between bounds
  df_samples <-
    gather_values(
      lower = lower,
      upper = upper,
      cluster_id = cluster_id,
      weighted = weighted,
      step_size = step_size,
      n_samples = n_samples
    ) |>
    dplyr::group_by(cluster_id) |>
    # compute the maximum density
    dplyr::mutate(
      median = median(.data$samples, na.rm = TRUE),
      q_05 = quantile(.data$samples, probs = .05),
      q_95 = quantile(.data$samples, probs = .95)
    ) |>
    dplyr::ungroup()

  # join samples with ground truth
  df_plot <-
    dplyr::full_join(
      df_samples,
      data.frame(
        truth = as.numeric(truth),
        cluster_id = cluster_id) |>
        dplyr::distinct()
    ) |>
    dplyr::mutate(cluster_id = factor(cluster_id))

  # plot
  plot <-
    ggplot_cumulative_intervals(
      data = df_plot,
      binwidth = step_size,
      min = min,
      max = max,
      facet_wrap = facet_wrap,
      show_quantiles = show_quantiles,
      ncol = ncol
    )

  return(plot)
}
