#' Plots two faceted violin plots of the percent of max age colonists on the
#' island for different values of mainland extinction. The left side facet has
#' the ideal data and the right side facet has the empirical data.
#'
#' @inheritParams default_params_doc
#'
#' @return Plot
#' @export
plot_max_age <- function(data_folder_path,
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

  max_age_list <- lapply(error_list, "[[", "max_age_percent")
  max_age_ideal_list <- lapply(max_age_list,
                               "[[",
                               "ideal_max_age")
  max_age_empirical_list <- lapply(max_age_list,
                                   "[[",
                                   "empirical_max_age")

  mainland_ex <- unlist(lapply(sim_params_list, "[[", "mainland_ex"))
  mainland_sample_prob <- unlist(lapply(sim_params_list, "[[",
                                        "mainland_sample_prob"))
  mainland_sample_type <- unlist(lapply(sim_params_list, "[[",
                                        "mainland_sample_type"))

  mainland_ex_list <- list()
  mainland_sample_prob_list <- list()
  mainland_sample_type_list <- list()
  for (i in seq_along(max_age_empirical_list)) {
    mainland_ex_list[[i]] <- rep(
      mainland_ex[i],
      length(max_age_empirical_list[[i]]))
    mainland_sample_prob_list[[i]] <- rep(
      mainland_sample_prob[i],
      length(max_age_empirical_list[[i]]))
    mainland_sample_type_list[[i]] <- rep(
      mainland_sample_type[i],
      length(max_age_empirical_list[[i]]))
  }

  plotting_data <- data.frame(
    max_age_ideal = unlist(max_age_ideal_list),
    max_age_empirical = unlist(max_age_empirical_list),
    mainland_ex = unlist(mainland_ex_list),
    mainland_sample_prob = unlist(mainland_sample_prob_list),
    mainland_sample_type = unlist(mainland_sample_type_list))

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
    ideal_max_age <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(mainland_ex),
                                         y = max_age_ideal),
                            fill = "#009E73",
                            outlier.size = 0.5,
                            lwd = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab("Ideal Max Age %") +
      ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M])))) +
      ggplot2::theme(text = ggplot2::element_text(size = 7.5)) +
      ggplot2::ylim(c(0, 100))

    empirical_max_age <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(mainland_ex),
                                         y = max_age_empirical),
                            fill = "#E69F00",
                            outlier.size = 0.5,
                            lwd = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab("Empirical Max Age %") +
      ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M])))) +
      ggplot2::theme(text = ggplot2::element_text(size = 7.5)) +
      ggplot2::ylim(c(0, 100))
  } else {
    ideal_max_age <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(mainland_sample_prob),
                                         y = max_age_ideal),
                            fill = "#009E73",
                            outlier.size = 0.5,
                            lwd = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab("Ideal Max Age %") +
      ggplot2::xlab(expression(paste("Mainland sampling probability ",
                                     (rho)))) +
      ggplot2::theme(text = ggplot2::element_text(size = 7.5)) +
      ggplot2::ylim(c(0, 100))

    empirical_max_age <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_boxplot(ggplot2::aes(x = as.factor(mainland_sample_prob),
                                         y = max_age_empirical),
                            fill = "#E69F00",
                            outlier.size = 0.5,
                            lwd = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab("Empirical Max Age %") +
      ggplot2::xlab(expression(paste("Mainland sampling probability ",
                                     (rho)))) +
      ggplot2::theme(text = ggplot2::element_text(size = 7.5)) +
      ggplot2::ylim(c(0, 100))
  }

  max_age <- cowplot::plot_grid(ideal_max_age, empirical_max_age,
                                labels = c("A", "B"),
                                label_size = 10)

  if (!is.null(output_file_path)) {
    ggplot2::ggsave(
      plot = max_age,
      filename = output_file_path,
      device = "png",
      width = 168,
      height = 100,
      units = "mm",
      dpi = 600
    )
  } else {
    return(max_age)
  }
}
