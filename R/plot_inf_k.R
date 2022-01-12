#' Plots the percentage of carrying capacity (K') estimates that are infinite
#'
#' @inheritParams default_params_doc
#'
#' @return Void (saves plot)
#' @export
plot_inf_k <- function(analysis_results,
                       output_file_path,
                       labels = NULL) {
  ideal_ml <- lapply(analysis_results, "[[", "ideal_ml")
  empirical_ml <- lapply(analysis_results, "[[", "empirical_ml")
  ideal_k <- lapply(ideal_ml, function(x) {
    unlist(lapply(x, "[[", "K"))
  })
  empirical_k <- lapply(empirical_ml, function(x) {
    unlist(lapply(x, "[[", "K"))
  })

  num_ideal_inf_k <- unlist(lapply(ideal_k, function(x) {
    length(which(x == Inf))
  }))
  num_empirical_inf_k <- unlist(lapply(empirical_k, function(x) {
    length(which(x == Inf))
  }))
  num_ideal_k <- unlist(lapply(ideal_k, length))
  num_empirical_k <- unlist(lapply(empirical_k, length))
  percent_ideal_k_inf <- (num_ideal_inf_k / num_ideal_k) * 100
  percent_empirical_k_inf <- (num_empirical_inf_k / num_empirical_k) * 100

  sim_params_list <- lapply(analysis_results, "[[", "sim_params")
  mainland_ex <- unlist(lapply(sim_params_list, "[[", "mainland_ex"))
  mainland_sample_prob <- unlist(lapply(
    sim_params_list, "[[",
    "mainland_sample_prob"
  ))
  mainland_sample_type <- unlist(lapply(
    sim_params_list, "[[",
    "mainland_sample_type"
  ))
  sim_k <- unlist(lapply(sim_params_list, "[[", "island_k"))

  plotting_data <- data.frame(
    percent_ideal_k_inf = percent_ideal_k_inf,
    percent_empirical_k_inf = percent_empirical_k_inf,
    mainland_ex = mainland_ex,
    mainland_sample_prob = mainland_sample_prob,
    mainland_sample_type = mainland_sample_type,
    sim_k = sim_k
  )

  plotting_data <-
    plotting_data[plotting_data$mainland_sample_type %in% "complete", ]

  plotting_data_k_5 <- plotting_data[plotting_data$sim_k %in% 5, ]
  plotting_data_k_50 <- plotting_data[plotting_data$sim_k %in% 50, ]

  percent_k_5_inf <- ggplot2::ggplot(data = plotting_data_k_5) +
    ggplot2::geom_point(ggplot2::aes(
      x = mainland_ex,
      y = percent_ideal_k_inf
    ),
    colour = "#009E73",
    shape = 16,
    alpha = 0.5,
    size = 3
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = mainland_ex,
      y = percent_empirical_k_inf
    ),
    colour = "#E69F00",
    shape = 17,
    alpha = 0.5,
    size = 3
    ) +
    ggplot2::theme_classic() +
    ggplot2::ylab("Percentage of Infinite K' (%)") +
    ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M]))))

  percent_k_50_inf <- ggplot2::ggplot(data = plotting_data_k_50) +
    ggplot2::geom_point(ggplot2::aes(
      x = mainland_ex,
      y = percent_ideal_k_inf
    ),
    colour = "#009E73",
    shape = 16,
    alpha = 0.5,
    size = 3
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = mainland_ex,
      y = percent_empirical_k_inf
    ),
    colour = "#E69F00",
    shape = 17,
    alpha = 0.5,
    size = 3
    ) +
    ggplot2::theme_classic() +
    ggplot2::ylab("Percentage of Infinite K' (%)") +
    ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M]))))

  k_5_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "True K' = 5"
    ) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )

  k_50_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "True K' = 50"
    ) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )

  k_5_plot <- cowplot::plot_grid(
    k_5_title,
    percent_k_5_inf,
    nrow = 2, rel_heights = c(0.1, 1)
  )

  k_50_plot <- cowplot::plot_grid(
    k_50_title,
    percent_k_50_inf,
    nrow = 2, rel_heights = c(0.1, 1)
  )

  k_inf_plot <- cowplot::plot_grid(
    k_5_plot,
    k_50_plot,
    nrow = 1,
    labels = labels,
    label_size = 10
  )

  if (!is.null(output_file_path)) {
    ggplot2::ggsave(
      plot = k_inf_plot,
      filename = output_file_path,
      device = "png",
      width = 160,
      height = 100,
      units = "mm",
      dpi = 600
    )
  } else {
    return(k_inf_plot)
  }
}
