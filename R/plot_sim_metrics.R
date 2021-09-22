#' Plots metrics from simulations
#'
#' @inheritParams default_params_doc
#'
#' @return none Four plots are shown: histogram of the number of species,
#' histogram of the number of colonizations, histogram of the largest clade size
#' and histogram of the rank of the largest clade
#' @author Joshua W. Lambert
#' @export plot_sim_metrics
plot_sim_metrics <- function(data_folder_path,
                             output_file_path) {

  files <- list.files(data_folder_path, pattern = "general")

  if (length(files) == 0) {
    stop("No results are in the results directory")
  } else {
    file_paths <- as.list(paste0(data_folder_path, "/", files))
    results_list <- lapply(file_paths, readRDS)
  }

  ideal_sim_metrics_list <- lapply(results_list, '[[', "ideal_sim_metrics")
  empirical_sim_metrics_list <- lapply(results_list, '[[', "empirical_sim_metrics")

  ideal_num_col_list <- lapply(ideal_sim_metrics_list, '[[', "num_col")
  ideal_num_spec_list <- lapply(ideal_sim_metrics_list, '[[', "num_spec")
  empirical_num_col_list <- lapply(empirical_sim_metrics_list, '[[', "num_col")
  empirical_num_spec_list <- lapply(empirical_sim_metrics_list, '[[', "num_spec")

  ideal_mean_num_col <- unlist(lapply(ideal_num_col_list, mean))
  ideal_var_num_col <- unlist(lapply(ideal_num_col_list, var))
  ideal_max_num_col <- unlist(lapply(ideal_num_col_list, max))
  ideal_min_num_col <- unlist(lapply(ideal_num_col_list, min))

  ideal_mean_num_spec <- unlist(lapply(ideal_num_spec_list, mean))
  ideal_var_num_spec <- unlist(lapply(ideal_num_spec_list, var))
  ideal_max_num_spec <- unlist(lapply(ideal_num_spec_list, max))
  ideal_min_num_spec <- unlist(lapply(ideal_num_spec_list, min))

  empirical_mean_num_col <- unlist(lapply(empirical_num_col_list, mean))
  empirical_var_num_col <- unlist(lapply(empirical_num_col_list, var))
  empirical_max_num_col <- unlist(lapply(empirical_num_col_list, max))
  empirical_min_num_col <- unlist(lapply(empirical_num_col_list, min))

  empirical_mean_num_spec <- unlist(lapply(empirical_num_spec_list, mean))
  empirical_var_num_spec <- unlist(lapply(empirical_num_spec_list, var))
  empirical_max_num_spec <- unlist(lapply(empirical_num_spec_list, max))
  empirical_min_num_spec <- unlist(lapply(empirical_num_spec_list, min))

  plotting_data <- data.frame(
    ideal_mean_num_col = ideal_mean_num_col,
    ideal_var_num_col = ideal_var_num_col,
    ideal_max_num_col = ideal_max_num_col,
    ideal_min_num_col = ideal_min_num_col,
    ideal_mean_num_spec = ideal_mean_num_spec,
    ideal_var_num_spec = ideal_var_num_spec,
    ideal_max_num_spec = ideal_max_num_spec,
    ideal_min_num_spec = ideal_min_num_spec,
    empirical_mean_num_col = empirical_mean_num_col,
    empirical_var_num_col = empirical_var_num_col,
    empirical_max_num_col = empirical_max_num_col,
    empirical_min_num_col = empirical_min_num_col,
    empirical_mean_num_spec = empirical_mean_num_spec,
    empirical_var_num_spec = empirical_var_num_spec,
    empirical_max_num_spec = empirical_max_num_spec,
    empirical_min_num_spec = empirical_min_num_spec)

  ideal_mean_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_mean_num_col),
                            fill = "#009E73",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Mean Number of Colonisation events") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  ideal_var_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_var_num_col),
                            fill = "#009E73",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Variance of Number of Colonisation events") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  ideal_max_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_max_num_col),
                            fill = "#009E73",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Maximum Number of Colonisation events") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  ideal_min_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_min_num_col),
                            fill = "#009E73",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Minimum Number of Colonisation events") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  ideal_mean_num_spec <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_mean_num_spec),
                            fill = "#009E73",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Mean Number of Species") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  ideal_var_num_spec <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_var_num_spec),
                            fill = "#009E73",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Variance of Number of Species") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  ideal_max_num_spec <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_max_num_spec),
                            fill = "#009E73",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Maximum Number of Species") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  ideal_min_num_spec <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_min_num_spec),
                            fill = "#009E73",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Minimum Number of Species") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  empirical_mean_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = empirical_mean_num_col),
                            fill = "#E69F00",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Mean Number of Colonisation events") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  empirical_var_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = empirical_var_num_col),
                            fill = "#E69F00",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Variance of Number of Colonisation events") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  empirical_max_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = empirical_max_num_col),
                            fill = "#E69F00",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Maximum Number of Colonisation events") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  empirical_min_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = empirical_min_num_col),
                            fill = "#E69F00",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Minimum Number of Colonisation events") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  empirical_mean_num_spec <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = empirical_mean_num_spec),
                            fill = "#E69F00",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Mean Number of Species") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  empirical_var_num_spec <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = empirical_var_num_spec),
                            fill = "#E69F00",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Variance of Number of Species") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  empirical_max_num_spec <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = empirical_max_num_spec),
                            fill = "#E69F00",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Maximum Number of Species") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  empirical_min_num_spec <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = empirical_min_num_spec),
                            fill = "#E69F00",
                            bins = 50) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Minimum Number of Species") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 5))

  ideal_sim_metrics <- cowplot::plot_grid(
    ideal_mean_num_col, ideal_var_num_col,
    ideal_max_num_col, ideal_min_num_col,
    ideal_mean_num_spec, ideal_var_num_spec,
    ideal_max_num_spec, ideal_min_num_spec,
    labels = "Ideal Data",
    label_size = 8,
    hjust = -1.5,
    nrow = 2)

  empirical_sim_metrics <- cowplot::plot_grid(
    empirical_mean_num_col, empirical_var_num_col,
    empirical_max_num_col, empirical_min_num_col,
    empirical_mean_num_spec, empirical_var_num_spec,
    empirical_max_num_spec, empirical_min_num_spec,
    labels = "Empirical Data",
    label_size = 8,
    hjust = -1.0,
    nrow = 2)

  sim_metrics <- cowplot::plot_grid(ideal_sim_metrics,
                                    empirical_sim_metrics,
                                    nrow = 2)

  if (!is.null(output_file_path)) {
    ggplot2::ggsave(
      plot = sim_metrics,
      filename = output_file_path,
      device = "png",
      width = 168,
      height = 100,
      units = "mm",
      dpi = 600
    )
  } else {
    return(sim_metrics)
  }
}
