#' Plots two faceted violin plots of the percent of endemics on the island for
#' different values of mainland extinction. The left side facet has the ideal
#' data and the right side facet has the empirical data.
#'
#' @inheritParams default_params_doc
#'
#' @return Plot
#' @export
plot_endemics <- function(data_folder_path,
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
  sim_params_list <- lapply(results_list, "[[", "sim_params")

  endemic_percent_list <- lapply(error_list, "[[", "endemic_percent")
  endemic_percent_empirical_list <- lapply(endemic_percent_list,
                                           "[[",
                                           "empirical_endemic_percent")
  endemic_percent_ideal_list <- lapply(endemic_percent_list,
                                       "[[",
                                       "ideal_endemic_percent")

  endemic_percent_empirical_means <- unlist(lapply(
    endemic_percent_empirical_list,
    mean))
  endemic_percent_ideal_means <- unlist(lapply(
    endemic_percent_ideal_list,
    mean))

  mainland_ex <- unlist(lapply(sim_params_list, "[[", "mainland_ex"))
  mainland_sample_prob <- unlist(lapply(sim_params_list, "[[",
                                        "mainland_sample_prob"))
  mainland_sample_type <- unlist(lapply(sim_params_list, "[[",
                                        "mainland_sample_type"))

  plotting_data <- data.frame(
    endemic_percent_empirical_means = endemic_percent_empirical_means,
    endemic_percent_ideal_means = endemic_percent_ideal_means,
    mainland_ex = as.factor(mainland_ex),
    mainland_sample_prob = as.factor(mainland_sample_prob),
    mainland_sample_type = mainland_sample_type)

  if (parameter == "mainland_ex") {
    plotting_data <- dplyr::filter(
      plotting_data,
      plotting_data$mainland_sample_type == "complete")
  } else if (parameter == "unsampled") {
    plotting_data <- dplyr::filter(
      plotting_data,
      plotting_data$mainland_sample_type == "unsampled")
  } else if (parameter == "undiscovered") {
    plotting_data <- dplyr::filter(
      plotting_data,
      plotting_data$mainland_sample_type == "undiscovered")
  }

  if (parameter == "mainland_ex") {
    ideal_endemics <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_violin(ggplot2::aes(x = mainland_ex,
                                        y = endemic_percent_ideal_means),
                           fill = "#009E73",
                           colour = "#009E73",
                           alpha = 0.3) +
      ggplot2::geom_boxplot(ggplot2::aes(x = mainland_ex,
                                         y = endemic_percent_ideal_means),
                            colour = "grey50",
                            width = 0.1,
                            alpha = 0.1) +
      ggplot2::theme_classic() +
      ggplot2::ylab("Mean Ideal Endemic Percent (%)") +
      ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M])))) +
      ggplot2::theme(text = ggplot2::element_text(size = 7.5)) +
      ggplot2::ylim(c(50, 100))

    empirical_endemics <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_violin(ggplot2::aes(x = mainland_ex,
                                        y = endemic_percent_empirical_means),
                           fill = "#E69F00",
                           colour = "#E69F00",
                           alpha = 0.3) +
      ggplot2::geom_boxplot(ggplot2::aes(x = mainland_ex,
                                         y = endemic_percent_empirical_means),
                            colour = "grey50",
                            width = 0.1,
                            alpha = 0.1) +
      ggplot2::theme_classic() +
      ggplot2::ylab("Mean Empirical Endemic Percent (%)") +
      ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M])))) +
      ggplot2::theme(text = ggplot2::element_text(size = 7.5)) +
      ggplot2::ylim(c(50, 100))
  } else {
    ideal_endemics <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_violin(ggplot2::aes(x = mainland_sample_prob,
                                        y = endemic_percent_ideal_means),
                           fill = "#009E73",
                           colour = "#009E73",
                           alpha = 0.3) +
      ggplot2::geom_boxplot(ggplot2::aes(x = mainland_sample_prob,
                                         y = endemic_percent_ideal_means),
                            colour = "grey50",
                            width = 0.1,
                            alpha = 0.1) +
      ggplot2::theme_classic() +
      ggplot2::ylab("Mean Ideal Endemic Percent (%)") +
      ggplot2::xlab(expression(paste("Mainland sampling probability ",
                                     (rho)))) +
      ggplot2::theme(text = ggplot2::element_text(size = 7.5)) +
      ggplot2::ylim(c(50, 100))

    empirical_endemics <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_violin(ggplot2::aes(x = mainland_sample_prob,
                                        y = endemic_percent_empirical_means),
                           fill = "#E69F00",
                           colour = "#E69F00",
                           alpha = 0.3) +
      ggplot2::geom_boxplot(ggplot2::aes(x = mainland_sample_prob,
                                         y = endemic_percent_empirical_means),
                            colour = "grey50",
                            width = 0.1,
                            alpha = 0.1) +
      ggplot2::theme_classic() +
      ggplot2::ylab("Mean Empirical Endemic Percent (%)") +
      ggplot2::xlab(expression(paste("Mainland sampling probability ",
                                     (rho)))) +
      ggplot2::theme(text = ggplot2::element_text(size = 7.5)) +
      ggplot2::ylim(c(50, 100))
  }

  endemics <- cowplot::plot_grid(ideal_endemics, empirical_endemics)

  if (!is.null(output_file_path)) {
    ggplot2::ggsave(
      plot = endemics,
      filename = output_file_path,
      device = "png",
      width = 168,
      height = 100,
      units = "mm",
      dpi = 600
    )
  } else {
    return(endemics)
  }
}
