#' Plots metrics from simulations
#'
#' @inheritParams default_params_doc
#'
#' @return none Four plots are shown: histogram of the number of species,
#' histogram of the number of colonizations, histogram of the largest clade size
#' and histogram of the rank of the largest clade
#' @author Joshua W. Lambert
#' @export plot_sim_metrics
plot_sim_metrics <- function(analysis_results,
                             output_file_path) {
  ideal_sim_metrics_list <- lapply(analysis_results, "[[", "ideal_sim_metrics")
  empirical_sim_metrics_list <- lapply(
    analysis_results,
    "[[",
    "empirical_sim_metrics"
  )

  ideal_num_col_list <- lapply(
    ideal_sim_metrics_list,
    "[[",
    "ideal_sim_num_col"
  )
  ideal_num_spec_list <- lapply(
    ideal_sim_metrics_list,
    "[[",
    "ideal_sim_num_spec"
  )
  empirical_num_col_list <- lapply(
    empirical_sim_metrics_list,
    "[[",
    "empirical_sim_num_col"
  )
  empirical_num_spec_list <- lapply(
    empirical_sim_metrics_list,
    "[[",
    "empirical_sim_num_spec"
  )

  ideal_mean_num_col <- unlist(lapply(ideal_num_col_list, mean))
  ideal_var_num_col <- unlist(lapply(ideal_num_col_list, stats::var))
  ideal_max_num_col <- unlist(lapply(ideal_num_col_list, max))
  ideal_min_num_col <- unlist(lapply(ideal_num_col_list, min))

  ideal_mean_num_spec <- unlist(lapply(ideal_num_spec_list, mean))
  ideal_var_num_spec <- unlist(lapply(ideal_num_spec_list, stats::var))
  ideal_max_num_spec <- unlist(lapply(ideal_num_spec_list, max))
  ideal_min_num_spec <- unlist(lapply(ideal_num_spec_list, min))

  empirical_mean_num_col <- unlist(lapply(empirical_num_col_list, mean))
  empirical_var_num_col <- unlist(lapply(empirical_num_col_list, stats::var))
  empirical_max_num_col <- unlist(lapply(empirical_num_col_list, max))
  empirical_min_num_col <- unlist(lapply(empirical_num_col_list, min))

  empirical_mean_num_spec <- unlist(lapply(empirical_num_spec_list, mean))
  empirical_var_num_spec <- unlist(lapply(empirical_num_spec_list, stats::var))
  empirical_max_num_spec <- unlist(lapply(empirical_num_spec_list, max))
  empirical_min_num_spec <- unlist(lapply(empirical_num_spec_list, min))

  testit::assert(identical(ideal_mean_num_spec, empirical_mean_num_spec))
  testit::assert(identical(ideal_var_num_spec, empirical_var_num_spec))
  testit::assert(identical(ideal_max_num_spec, empirical_max_num_spec))
  testit::assert(identical(ideal_min_num_spec, empirical_min_num_spec))

  plotting_data <- data.frame(
    ideal_mean_num_col = ideal_mean_num_col,
    ideal_var_num_col = ideal_var_num_col,
    ideal_max_num_col = ideal_max_num_col,
    ideal_min_num_col = ideal_min_num_col,
    empirical_mean_num_col = empirical_mean_num_col,
    empirical_var_num_col = empirical_var_num_col,
    empirical_max_num_col = empirical_max_num_col,
    empirical_min_num_col = empirical_min_num_col,
    ideal_mean_num_spec = ideal_mean_num_spec,
    ideal_var_num_spec = ideal_var_num_spec,
    ideal_max_num_spec = ideal_max_num_spec,
    ideal_min_num_spec = ideal_min_num_spec
  )

  ideal_mean_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_mean_num_col),
      fill = "#009E73",
      bins = 8
    ) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Mean Number of Colonisations") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 6))

  ideal_var_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_var_num_col),
      fill = "#009E73",
      bins = 8
    ) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Variance of Number of Colonisations") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 6))

  ideal_max_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_max_num_col),
      fill = "#009E73",
      bins = 8
    ) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Maximum Number of Colonisations") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 6))

  ideal_min_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_min_num_col),
      fill = "#009E73",
      bins = 8
    ) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Minimum Number of Colonisations") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 6))

  empirical_mean_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = empirical_mean_num_col),
      fill = "#E69F00",
      bins = 8
    ) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Mean Number of Colonisations") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 6))

  empirical_var_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = empirical_var_num_col),
      fill = "#E69F00",
      bins = 8
    ) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Variance of Number of Colonisations") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 6))

  empirical_max_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = empirical_max_num_col),
      fill = "#E69F00",
      bins = 8
    ) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Maximum Number of Colonisations") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 6))

  empirical_min_num_col <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = empirical_min_num_col),
      fill = "#E69F00",
      bins = 8
    ) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Minimum Number of Colonisations") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 6))

  mean_num_spec <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_mean_num_spec),
      fill = "#E34234",
      bins = 8
    ) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Mean Number of Species") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 6))

  var_num_spec <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_var_num_spec),
      fill = "#E34234",
      bins = 8
    ) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Variance of Number of Species") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 6))

  max_num_spec <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_max_num_spec),
      fill = "#E34234",
      bins = 8
    ) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Maximum Number of Species") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 6))

  min_num_spec <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_histogram(ggplot2::aes(x = ideal_min_num_spec),
      fill = "#E34234",
      bins = 8
    ) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Minimum Number of Species") +
    ggplot2::ylab("Frequency") +
    ggplot2::theme(text = ggplot2::element_text(size = 6))

  sim_metrics <- cowplot::plot_grid(
    ideal_mean_num_col, ideal_var_num_col,
    ideal_max_num_col, ideal_min_num_col,
    empirical_mean_num_col, empirical_var_num_col,
    empirical_max_num_col, empirical_min_num_col,
    mean_num_spec, var_num_spec,
    max_num_spec, min_num_spec,
    nrow = 3
  )

  if (!is.null(output_file_path)) {
    ggplot2::ggsave(
      plot = sim_metrics,
      filename = output_file_path,
      device = "png",
      width = 180,
      height = 100,
      units = "mm",
      dpi = 600
    )
  } else {
    return(sim_metrics)
  }
}
