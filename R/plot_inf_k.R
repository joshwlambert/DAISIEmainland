#' Plots the percentage of carrying capacity (K') estimates that are infinite
#'
#' @inheritParams default_params_doc
#'
#' @return Void (saves plot)
#' @export
plot_inf_k <- function(data_folder_path,
                               output_file_path,
                               param_space,
                               parameter) {

  testit::assert(
    "Param_space must be either 'general', 'mainland_ex' or
    'mainland_sample_prob'",
    param_space == "general" || param_space == "mainland_ex" ||
      param_space == "mainland_sample_prob")

  files <- list.files(data_folder_path, pattern = param_space)

  if (length(files) == 0) {
    stop("No results are in the results directory")
  } else {
    file_paths <- as.list(paste0(data_folder_path, "/", files))
    results_list <- lapply(file_paths, readRDS)
  }

  error_list <- lapply(results_list, "[[", "error")
  ideal_ml <- lapply(results_list, "[[", "ideal_ml")
  empirical_ml <- lapply(results_list, "[[", "empirical_ml")
  ideal_k <- lapply(ideal_ml, function(x) {
    unlist(lapply(x, '[[', "K"))
  })
  empirical_k <- lapply(empirical_ml, function(x) {
    unlist(lapply(x, '[[', "K"))
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

  sim_params_list <- lapply(results_list, "[[", "sim_params")
  mainland_ex <- unlist(lapply(sim_params_list, "[[", 6))
  mainland_sample_prob <- unlist(lapply(sim_params_list, "[[", 7))
  sim_k <- unlist(lapply(sim_params_list, "[[", "island_k"))

  plotting_data <- data.frame(
    percent_ideal_k_inf = percent_ideal_k_inf,
    percent_empirical_k_inf = percent_empirical_k_inf,
    mainland_ex = mainland_ex,
    mainland_sample_prob = mainland_sample_prob,
    sim_k = sim_k)

  if (parameter == "mainland_ex") {
    plotting_data <- dplyr::filter(
      plotting_data,
      plotting_data$mainland_sample_prob == 1.0)
  } else {
    plotting_data <- dplyr::filter(
      plotting_data,
      plotting_data$mainland_ex == 0.0)
  }

  plotting_data_k_10 <- dplyr::filter(
    plotting_data,
    plotting_data$sim_k == 10)

  plotting_data_k_100 <- dplyr::filter(
    plotting_data,
    plotting_data$sim_k == 100)

  percent_k_10_inf <- ggplot2::ggplot(data = plotting_data_k_10) +
    ggplot2::geom_point(ggplot2::aes(x = mainland_ex,
                                     y = percent_ideal_k_inf),
                        colour = "#009E73",
                        shape = 16,
                        alpha = 0.5,
                        size = 3) +
    ggplot2::geom_point(ggplot2::aes(x = mainland_ex,
                                     y = percent_empirical_k_inf),
                        colour = "#E69F00",
                        shape = 17,
                        alpha = 0.5,
                        size = 3) +
    ggplot2::theme_classic() +
    ggplot2::ylab("Percentage of Infinite Ideal K' (%)") +
    ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M]))))

  percent_k_100_inf <- ggplot2::ggplot(data = plotting_data_k_100) +
    ggplot2::geom_point(ggplot2::aes(x = mainland_ex,
                                     y = percent_ideal_k_inf),
                        colour = "#009E73",
                        shape = 16,
                        alpha = 0.5,
                        size = 3) +
    ggplot2::geom_point(ggplot2::aes(x = mainland_ex,
                                     y = percent_empirical_k_inf),
                        colour = "#E69F00",
                        shape = 17,
                        alpha = 0.5,
                        size = 3) +
    ggplot2::theme_classic() +
    ggplot2::ylab("Percentage of Infinite Ideal K' (%)") +
    ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M]))))

  k_10_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "True K' = 10") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0))

  k_100_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "True K' = 100") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0))

  k_10_plot <- cowplot::plot_grid(
    k_10_title,
    percent_k_10_inf,
    nrow = 2, rel_heights = c(0.1, 1))

  k_100_plot <- cowplot::plot_grid(
    k_100_title,
    percent_k_100_inf,
    nrow = 2, rel_heights = c(0.1, 1))

  k_inf_plot <- cowplot::plot_grid(k_10_plot, k_100_plot, nrow = 1)

  if (!is.null(output_file_path)) {
    ggplot2::ggsave(
      plot = k_inf_plot,
      filename = output_file_path,
      device = "png",
      width = 168,
      height = 100,
      units = "mm",
      dpi = 600
    )
  } else {
    return(k_inf_plot)
  }

}
