#' Boxplots of the estimated carrying capacity faceted for ideal and empirical
#' data for two different values of K'.
#'
#' @inheritParams default_params_doc
#'
#' @return Void (saves plot)
#' @export
plot_k_estimates <- function(analysis_results,
                             output_file_path,
                             parameter,
                             num_breaks,
                             signif,
                             scientific,
                             labels = NULL) {

  testit::assert(
    "Parameter must be either 'mainland_ex', 'unsampled' or 'undiscovered'",
    parameter == "mainland_ex" || parameter == "unsampled" ||
      parameter == "undiscovered")

  ideal_ml <- lapply(analysis_results, "[[", "ideal_ml")
  empirical_ml <- lapply(analysis_results, "[[", "empirical_ml")
  ideal_k <- lapply(ideal_ml, function(x) {
    unlist(lapply(x, "[[", "K"))
  })
  empirical_k <- lapply(empirical_ml, function(x) {
    unlist(lapply(x, "[[", "K"))
  })

  ideal_k_no_inf <- lapply(ideal_k, function(x) {
    temp_x <- x[!is.infinite(x)]
    temp_x <- temp_x[!is.nan(temp_x)]
    return(temp_x)
  })

  empirical_k_no_inf <- lapply(empirical_k, function(x) {
    temp_x <- x[!is.infinite(x)]
    temp_x <- temp_x[!is.nan(temp_x)]
    return(temp_x)
  })

  sim_params_list <- lapply(analysis_results, "[[", "sim_params")
  mainland_ex <- lapply(sim_params_list, "[[", "mainland_ex")
  mainland_sample_prob <- lapply(sim_params_list, "[[", "mainland_sample_prob")
  mainland_sample_type <- lapply(sim_params_list, "[[", "mainland_sample_type")
  sim_k <- lapply(sim_params_list, "[[", "island_k")

  ideal_mainland_ex <- list()
  ideal_mainland_sample_prob <- list()
  ideal_mainland_sample_type <- list()
  ideal_sim_k <- list()
  for (i in seq_along(ideal_k_no_inf)) {
    ideal_mainland_ex[[i]] <- rep(mainland_ex[[i]],
                                  length(ideal_k_no_inf[[i]]))
    ideal_mainland_sample_prob[[i]] <- rep(mainland_sample_prob[[i]],
                                          length(ideal_k_no_inf[[i]]))
    ideal_mainland_sample_type[[i]] <- rep(mainland_sample_type[[i]],
                                           length(ideal_k_no_inf[[i]]))
    ideal_sim_k[[i]] <- rep(sim_k[[i]], length(ideal_k_no_inf[[i]]))
  }

  empirical_mainland_ex <- list()
  empirical_mainland_sample_prob <- list()
  empirical_mainland_sample_type <- list()
  empirical_sim_k <- list()
  for (i in seq_along(empirical_k_no_inf)) {
    empirical_mainland_ex[[i]] <- rep(mainland_ex[[i]],
                                      length(empirical_k_no_inf[[i]]))
    empirical_mainland_sample_prob[[i]] <- rep(mainland_sample_prob[[i]],
                                               length(empirical_k_no_inf[[i]]))
    empirical_mainland_sample_type[[i]] <- rep(mainland_sample_type[[i]],
                                               length(empirical_k_no_inf[[i]]))
    empirical_sim_k[[i]] <- rep(sim_k[[i]], length(empirical_k_no_inf[[i]]))
  }

  ideal_plotting_data <- data.frame(
    ideal_k = unlist(ideal_k_no_inf),
    mainland_ex = unlist(ideal_mainland_ex),
    mainland_sample_prob = unlist(ideal_mainland_sample_prob),
    mainland_sample_type = unlist(ideal_mainland_sample_type),
    sim_k = unlist(ideal_sim_k))

  empirical_plotting_data <- data.frame(
    empirical_k = unlist(empirical_k_no_inf),
    mainland_ex = unlist(empirical_mainland_ex),
    mainland_sample_prob = unlist(empirical_mainland_sample_prob),
    mainland_sample_type = unlist(empirical_mainland_sample_type),
    sim_k = unlist(empirical_sim_k))

  if (parameter == "mainland_ex") {
    which_ideal <- ideal_plotting_data$mainland_sample_type %in% "complete"
    ideal_plotting_data <- ideal_plotting_data[which_ideal, ]
    which_empirical <-
      empirical_plotting_data$mainland_sample_type %in% "complete"
    empirical_plotting_data <- empirical_plotting_data[which_empirical, ]
  } else {
    which_ideal <- ideal_plotting_data$mainland_sample_type %in% parameter
    ideal_plotting_data <- ideal_plotting_data[which_ideal, ]
    which_empirical <-
      empirical_plotting_data$mainland_sample_type %in% parameter
    empirical_plotting_data <- empirical_plotting_data[which_empirical, ]
  }

  ideal_plotting_data_k_5 <-
    ideal_plotting_data[ideal_plotting_data$sim_k %in% 5, ]
  empirical_plotting_data_k_5 <-
    empirical_plotting_data[empirical_plotting_data$sim_k %in% 5, ]

  upper_k_5_ylim <- max(ideal_plotting_data_k_5$ideal_k,
                        empirical_plotting_data_k_5$empirical_k)

  ideal_plotting_data_k_50 <-
    ideal_plotting_data[ideal_plotting_data$sim_k %in% 50, ]
  empirical_plotting_data_k_50 <-
    empirical_plotting_data[empirical_plotting_data$sim_k %in% 50, ]

  upper_k_50_ylim <- max(ideal_plotting_data_k_50$ideal_k,
                         empirical_plotting_data_k_50$empirical_k)

  if (parameter == "mainland_ex") {
    ideal_k_5 <- ggplot2::ggplot(data = ideal_plotting_data_k_5,
                                 ggplot2::aes(
                                   x = as.factor(mainland_ex),
                                   y = ideal_k)) +
      ggplot2::stat_summary(fun.data = calc_quantiles,
                            geom = "boxplot",
                            fill = "#009E73",
                            lwd = 0.5) +
      ggplot2::stat_summary(fun = calc_outliers,
                            geom = "point",
                            size = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[I])) +
      ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M])))) +
      ggplot2::geom_hline(yintercept = 5, colour = "grey50") +
      ggplot2::scale_y_continuous(
        breaks = scales::breaks_log(num_breaks, base = exp(1)),
        labels = create_labels(signif = signif,
                               scientific = scientific),
        trans = "log")

    empirical_k_5 <- ggplot2::ggplot(data = empirical_plotting_data_k_5,
                                 ggplot2::aes(
                                   x = as.factor(mainland_ex),
                                   y = empirical_k)) +
      ggplot2::stat_summary(fun.data = calc_quantiles,
                            geom = "boxplot",
                            fill = "#E69F00",
                            lwd = 0.5) +
      ggplot2::stat_summary(fun = calc_outliers,
                            geom = "point",
                            size = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[E])) +
      ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M])))) +
      ggplot2::geom_hline(yintercept = 5, colour = "grey50") +
      ggplot2::scale_y_continuous(
        breaks = scales::breaks_log(num_breaks, base = exp(1)),
        labels = create_labels(signif = signif,
                               scientific = scientific),
        trans = "log")

    ideal_k_50 <- ggplot2::ggplot(data = ideal_plotting_data_k_50,
                                 ggplot2::aes(
                                   x = as.factor(mainland_ex),
                                   y = ideal_k)) +
      ggplot2::stat_summary(fun.data = calc_quantiles,
                            geom = "boxplot",
                            fill = "#009E73",
                            lwd = 0.5) +
      ggplot2::stat_summary(fun = calc_outliers,
                            geom = "point",
                            size = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[I])) +
      ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M])))) +
      ggplot2::geom_hline(yintercept = 50, colour = "grey50") +
      ggplot2::scale_y_continuous(
        breaks = scales::breaks_log(num_breaks, base = exp(1)),
        labels = create_labels(signif = signif,
                               scientific = scientific),
        trans = "log")

    empirical_k_50 <- ggplot2::ggplot(data = empirical_plotting_data_k_50,
                                     ggplot2::aes(
                                       x = as.factor(mainland_ex),
                                       y = empirical_k)) +
      ggplot2::stat_summary(fun.data = calc_quantiles,
                            geom = "boxplot",
                            fill = "#E69F00",
                            lwd = 0.5) +
      ggplot2::stat_summary(fun = calc_outliers,
                            geom = "point",
                            size = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[E])) +
      ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M])))) +
      ggplot2::geom_hline(yintercept = 50, colour = "grey50") +
      ggplot2::scale_y_continuous(
        breaks = scales::breaks_log(num_breaks, base = exp(1)),
        labels = create_labels(signif = signif,
                               scientific = scientific),
        trans = "log")
  } else {
    ideal_k_5 <- ggplot2::ggplot(data = ideal_plotting_data_k_5,
                                 ggplot2::aes(
                                   x = as.factor(mainland_sample_prob),
                                   y = ideal_k)) +
      ggplot2::stat_summary(fun.data = calc_quantiles,
                            geom = "boxplot",
                            fill = "#009E73",
                            lwd = 0.5) +
      ggplot2::stat_summary(fun = calc_outliers,
                            geom = "point",
                            size = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[I])) +
      ggplot2::xlab(expression(paste("Mainland sampling probability ",
                                     (rho)))) +
      ggplot2::geom_hline(yintercept = 5, colour = "grey50") +
      ggplot2::scale_y_continuous(
        breaks = scales::breaks_log(num_breaks, base = exp(1)),
        labels = create_labels(signif = signif,
                               scientific = scientific),
        trans = "log")

    empirical_k_5 <- ggplot2::ggplot(data = empirical_plotting_data_k_5,
                                     ggplot2::aes(
                                       x = as.factor(mainland_sample_prob),
                                       y = empirical_k)) +
      ggplot2::stat_summary(fun.data = calc_quantiles,
                            geom = "boxplot",
                            fill = "#E69F00",
                            lwd = 0.5) +
      ggplot2::stat_summary(fun = calc_outliers,
                            geom = "point",
                            size = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[E])) +
      ggplot2::xlab(expression(paste("Mainland sampling probability ",
                                     (rho)))) +
      ggplot2::geom_hline(yintercept = 5, colour = "grey50") +
      ggplot2::scale_y_continuous(
        breaks = scales::breaks_log(num_breaks, base = exp(1)),
        labels = create_labels(signif = signif,
                               scientific = scientific),
        trans = "log")

    ideal_k_50 <- ggplot2::ggplot(data = ideal_plotting_data_k_50,
                                  ggplot2::aes(
                                    x = as.factor(mainland_sample_prob),
                                    y = ideal_k)) +
      ggplot2::stat_summary(fun.data = calc_quantiles,
                            geom = "boxplot",
                            fill = "#009E73",
                            lwd = 0.5) +
      ggplot2::stat_summary(fun = calc_outliers,
                            geom = "point",
                            size = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[I])) +
      ggplot2::xlab(expression(paste("Mainland sampling probability ",
                                     (rho)))) +
      ggplot2::geom_hline(yintercept = 50, colour = "grey50") +
      ggplot2::scale_y_continuous(
        breaks = scales::breaks_log(num_breaks, base = exp(1)),
        labels = create_labels(signif = signif,
                               scientific = scientific),
        trans = "log")

    empirical_k_50 <- ggplot2::ggplot(data = empirical_plotting_data_k_50,
                                      ggplot2::aes(
                                        x = as.factor(mainland_sample_prob),
                                        y = empirical_k)) +
      ggplot2::stat_summary(fun.data = calc_quantiles,
                            geom = "boxplot",
                            fill = "#E69F00",
                            lwd = 0.5) +
      ggplot2::stat_summary(fun = calc_outliers,
                            geom = "point",
                            size = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[E])) +
      ggplot2::xlab(expression(paste("Mainland sampling probability ",
                                     (rho)))) +
      ggplot2::geom_hline(yintercept = 50, colour = "grey50") +
      ggplot2::scale_y_continuous(
        breaks = scales::breaks_log(num_breaks, base = exp(1)),
        labels = create_labels(signif = signif,
                               scientific = scientific),
        trans = "log")
  }

  k_5_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "True K' = 5",
      size = 12) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 0, 250))

  k_50_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "True K' = 50",
      size = 12) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 0, 250))

  k_plot <- cowplot::plot_grid(
    k_5_title, NULL,
    ideal_k_5, empirical_k_5,
    k_50_title, NULL,
    ideal_k_50, empirical_k_50,
    nrow = 4,
    rel_heights = c(0.1, 1, 0.1, 1),
    labels = labels,
    label_size = 10)

  if (!is.null(output_file_path)) {
    ggplot2::ggsave(
      plot = k_plot,
      filename = output_file_path,
      device = "png",
      width = 180,
      height = 80,
      units = "mm",
      dpi = 600
    )
  } else {
    return(k_plot)
  }
}
