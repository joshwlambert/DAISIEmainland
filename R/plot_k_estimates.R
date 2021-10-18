#' Plots carrying capacity estimates
#'
#' @inheritParams default_params_doc
#'
#' @return Void (saves plot)
#' @export
plot_k_estimates <- function(data_folder_path,
                             output_file_path,
                             parameter) {

  testit::assert(
    "Parameter must be either 'mainland_ex', 'unsampled' or 'undiscovered'",
    parameter == "mainland_ex" || parameter == "unsampled" ||
      parameter == "undiscovered")

  files <- list.files(data_folder_path)

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

  ideal_k_no_inf <- lapply(ideal_k, function(x) {
    temp_x <- x[!is.infinite(x)]
    temp_x <- temp_x[!is.nan(temp_x)]
    temp_x <- asinh(temp_x)
    return(temp_x)
  })

  empirical_k_no_inf <- lapply(empirical_k, function(x) {
    temp_x <- x[!is.infinite(x)]
    temp_x <- temp_x[!is.nan(temp_x)]
    temp_x <- asinh(temp_x)
    return(temp_x)
  })

  sim_params_list <- lapply(results_list, "[[", "sim_params")
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
    ideal_plotting_data <- dplyr::filter(
      ideal_plotting_data,
      ideal_plotting_data$mainland_sample_type == "complete")
    empirical_plotting_data <- dplyr::filter(
      empirical_plotting_data,
      empirical_plotting_data$mainland_sample_type == "complete")
  } else if (parameter == "unsampled") {
    ideal_plotting_data <- dplyr::filter(
      ideal_plotting_data,
      ideal_plotting_data$mainland_sample_type == "unsampled")
    empirical_plotting_data <- dplyr::filter(
      empirical_plotting_data,
      empirical_plotting_data$mainland_sample_type == "unsampled")
  } else if (parameter == "undiscovered") {
    ideal_plotting_data <- dplyr::filter(
      ideal_plotting_data,
      ideal_plotting_data$mainland_sample_type == "undiscovered")
    empirical_plotting_data <- dplyr::filter(
      empirical_plotting_data,
      empirical_plotting_data$mainland_sample_type == "undiscovered")
  }

  ideal_plotting_data_k_5 <- dplyr::filter(
    ideal_plotting_data,
    ideal_plotting_data$sim_k == 5)
  empirical_plotting_data_k_5 <- dplyr::filter(
    empirical_plotting_data,
    empirical_plotting_data$sim_k == 5)

  ideal_plotting_data_k_50 <- dplyr::filter(
    ideal_plotting_data,
    ideal_plotting_data$sim_k == 50)
  empirical_plotting_data_k_50 <- dplyr::filter(
    empirical_plotting_data,
    empirical_plotting_data$sim_k == 50)

  if (parameter == "mainland_ex") {
    ideal_k_5 <- ggplot2::ggplot(data = ideal_plotting_data_k_5) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(mainland_ex),
                                         y = ideal_k),
                            fill = "#009E73",
                            outlier.size = 0.5,
                            lwd = 0.25) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[I])) +
      ggplot2::xlab(expression(mu[M]))

    empirical_k_5 <- ggplot2::ggplot(data = empirical_plotting_data_k_5) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(mainland_ex),
                                         y = empirical_k),
                            fill = "#E69F00",
                            outlier.size = 0.5,
                            lwd = 0.25) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[E])) +
      ggplot2::xlab(expression(mu[M]))

    ideal_k_50 <- ggplot2::ggplot(data = ideal_plotting_data_k_50) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(mainland_ex),
                                         y = ideal_k),
                            fill = "#009E73",
                            outlier.size = 0.5,
                            lwd = 0.25) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[I])) +
      ggplot2::xlab(expression(mu[M]))

    empirical_k_50 <- ggplot2::ggplot(data = empirical_plotting_data_k_50) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(mainland_ex),
                                         y = empirical_k),
                            fill = "#E69F00",
                            outlier.size = 0.5,
                            lwd = 0.25) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[E])) +
      ggplot2::xlab(expression(mu[M]))
  } else {
    ideal_k_5 <- ggplot2::ggplot(data = ideal_plotting_data_k_5) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(mainland_sample_prob),
                                         y = ideal_k),
                            fill = "#009E73",
                            outlier.size = 0.5,
                            lwd = 0.25) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[I])) +
      ggplot2::xlab(expression(paste("Mainland sampling probability ", (rho))))

    empirical_k_5 <- ggplot2::ggplot(data = empirical_plotting_data_k_5) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(mainland_sample_prob),
                                         y = empirical_k),
                            fill = "#E69F00",
                            outlier.size = 0.5,
                            lwd = 0.25) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[E])) +
      ggplot2::xlab(expression(paste("Mainland sampling probability ", (rho))))

    ideal_k_50 <- ggplot2::ggplot(data = ideal_plotting_data_k_50) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(mainland_sample_prob),
                                         y = ideal_k),
                            fill = "#009E73",
                            outlier.size = 0.5,
                            lwd = 0.25) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[I])) +
      ggplot2::xlab(expression(paste("Mainland sampling probability ", (rho))))

    empirical_k_50 <- ggplot2::ggplot(data = empirical_plotting_data_k_50) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(mainland_sample_prob),
                                         y = empirical_k),
                            fill = "#E69F00",
                            outlier.size = 0.5,
                            lwd = 0.25) +
      ggplot2::theme_classic() +
      ggplot2::ylab(expression("K'"[E])) +
      ggplot2::xlab(expression(paste("Mainland sampling probability ", (rho))))
  }

  k_5_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "True K' = 5") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 0, 250))

  k_50_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "True K' = 50") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0, 0, 0, 250))

  k_5_plot <- cowplot::plot_grid(
    k_5_title, NULL,
    ideal_k_5, empirical_k_5,
    nrow = 2, rel_heights = c(0.1, 1))

  k_50_plot <- cowplot::plot_grid(
    k_50_title, NULL,
    ideal_k_50, empirical_k_50,
    nrow = 2, rel_heights = c(0.1, 1))

  k_plot <- cowplot::plot_grid(k_5_plot, k_50_plot, nrow = 2)

  if (!is.null(output_file_path)) {
    ggplot2::ggsave(
      plot = k_plot,
      filename = output_file_path,
      device = "png",
      width = 168,
      height = 100,
      units = "mm",
      dpi = 600
    )
  } else {
    return(k_plot)
  }
}
