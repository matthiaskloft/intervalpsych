

plot_intervals_cumulative <- function(lower,
                                      upper,
                                      cluster_id,
                                      cluster_label = NA,
                                      truth = NA,
                                      min,
                                      max,
                                      facet_wrap = FALSE,
                                      design = NULL,
                                      step_size,
                                      show_quantiles = TRUE,
                                      ncol = 2) {

  # gather values between bounds
  gathered <-
    gather_values(lower, upper, cluster_id, step_size = step_size) %>%
    group_by(cluster_id) %>%
    # compute the maximum density
    mutate(
      max_density = max(table(round(
        samples, -floor(log10(step_size))
      )), na.rm = TRUE) %>%
        as.double(),
      median = median(samples, na.rm = TRUE),
      q_05 = quantile(samples, probs = .05),
      q_95 = quantile(samples, probs = .95)
    ) %>%
    ungroup() %>%
    full_join(
      data.frame(
        lower_mean = lower_mean,
        upper_mean = upper_mean,
        lower_mean_logit = lower_mean_logit,
        upper_mean_logit = upper_mean_logit,
        truth = as.numeric(truth),
        cluster_id = cluster_id,
        item_name = item_name
      ) %>%
        distinct()
    ) %>%
    mutate(cluster_id = factor(cluster_id),
           item_name = factor(item_name))

  plot <-
    try(ggplot_aggregated_single(
      data = gathered,
      binwidth = step_size,
      min = min,
      max = max,
      facet_wrap = facet_wrap,
      design = design,
      show_quantiles = show_quantiles,
      ncol = ncol
    ))
  return(plot)
}

# helper: gather interval values -----------------------------------------------
gather_values <- function(lower, upper, cluster_id, step_size) {
  # identify columns for lower and upper response value


  df_out <- map_dfr(1:length(lower), function(.x) {
    # gather values between bounds by step size
    samples <- seq(lower[.x], upper[.x], by = step_size) %>% as.double()
    cluster_id <- rep(cluster_id[.x], length(samples))

    return(data.frame(samples = samples, cluster_id = cluster_id))
  })

  return(df_out %>% arrange(cluster_id))
}

# helper: ggplot for aggregated responses --------------------------------------
ggplot_aggregated_single <-
  function(data,
           min = min,
           max = max,
           binwidth,
           facet_wrap = FALSE,
           design = NULL,
           show_quantiles = TRUE,
           ncol = 2) {
    # avoid warnings
    #samples <- NULL

    # plot
    plot <-
      ggplot2::ggplot(data, ggplot2::aes(x = samples)) +

      ggplot2::geom_area(
        stat = "bin",
        color = "black",
        fill = "gray95",
        binwidth = binwidth,
        linewidth = .7
      ) +
      ggplot2::geom_vline(
        aes(xintercept = truth),
        color = ggokabeito::palette_okabe_ito(order = 1),
        line_width = 1
      ) +
      ggplot2::geom_errorbarh(
        ggplot2::aes(
          xmin = lower_mean_logit,
          xmax = upper_mean_logit,
          y = max_density * 1.15,
          height = max_density * .05
        ),
        col = "grey70",
        height = 0,
        linetype = 1,
        linewidth = 8
      ) +
      ggplot2::geom_errorbarh(
        ggplot2::aes(
          xmin = lower_mean,
          xmax = upper_mean,
          y = max_density * 1.15,
          height = max_density * .1
        ),
        col = "black",
        linetype = 1,
        linewidth = 1.5
      ) +
      ggplot2::scale_x_continuous(
        limits = c(min, max),
        labels = c(c("0", ".25", ".50", ".75", "1")),
        breaks = seq(min, max, length.out = 5),
        expand = ggplot2::expansion()
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .05))) +
      ggplot2::labs(x = "Response Value", y = "Cumulative Pointwise Frequency") +
      theme_itm() +
      theme(
        plot.margin = margin(.2, .5, .2, .2, "cm"),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "#6d6d6e", size = .3),
        axis.ticks = element_line(colour = "#6d6d6e", size = .3),
        axis.text = element_text(size = 10),
        strip.text = element_text(margin = margin(0, 0, .3, 0, "cm")),
      ) +

      ggplot2::theme(#strip.text = ggplot2::element_text(size = 14),
        #strip.background = ggplot2::element_rect(fill = "gray95"),
        #strip.clip = "on")
        # add facet wrap
        if (facet_wrap == TRUE) {
          if (is.null(design)) {
            plot <- plot + ggplot2::facet_wrap( ~ item_name, scales = "free", ncol = ncol)
          } else {
            plot <- plot +
              facet_manual( ~ item_name, design = design, scales = "free")
          }
        }

        if (show_quantiles == TRUE) {
          # add quantiles to the plot
          plot <- plot +
            ggplot2::geom_vline(
              ggplot2::aes(xintercept = median),
              col = "black",
              linetype = 1,
              linewidth = .6
            ) +
            ggplot2::geom_vline(
              ggplot2::aes(xintercept = q_05),
              col = "black",
              linetype = 2,
              linewidth = .7
            ) +
            ggplot2::geom_vline(
              ggplot2::aes(xintercept = q_95),
              col = "black",
              linetype = 2,
              linewidth = .7
            )
        }
        return(plot)
  }
