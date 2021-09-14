#' Plots two faceted violin plots of the percent of max age colonists on the
#' island for different values of mainland extinction. The left side facet has
#' the ideal data and the right side facet has the empirical data.
#'
#' @return Plot
#' @export
plot_max_age <- function() {

  files <- list.files(file.path(getwd(), "results"))

  if (length(files) == 0) {
    stop("No results are in the results directory")
  } else {
    file_paths <- as.list(paste0(file.path(getwd(), "results"), "/", files))
    results_list <- lapply(file_paths, readRDS)
  }

  error_list <- lapply(results_list, '[[', "error")
  sim_params_list <- lapply(results_list, '[[', "sim_params")

  max_age_ratio_list <- lapply(error_list, '[[', "max_age_ratio")
  max_age_ratio_ideal_list <- lapply(max_age_ratio_list, '[[', "ideal_max_age")
  max_age_ratio_empirical_list <- lapply(max_age_ratio_list,
                                         '[[',
                                         "empirical_max_age")
  max_age_ratio_ideal_means <- unlist(lapply(max_age_ratio_ideal_list, mean))
  max_age_ratio_empirical_means <- unlist(lapply(max_age_ratio_empirical_list,
                                                 mean))

  mainland_ex <- unlist(lapply(sim_params_list, '[[', 6))
  mainland_sample_prob <- unlist(lapply(sim_params_list, '[[', 7))

  plotting_data <- data.frame(
    max_age_ratio_ideal_means = max_age_ratio_ideal_means,
    max_age_ratio_empirical_means = max_age_ratio_empirical_means,
    mainland_ex = as.factor(mainland_ex),
    mainland_sample_prob = as.factor(mainland_sample_prob))

  ideal_max_age <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_violin(ggplot2::aes(x = mainland_ex,
                                      y = max_age_ratio_ideal_means),
                         fill = "#009E73",
                         colour = "#009E73",
                         alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::ylab("Mean Ideal Max Age Percent (%)") +
    ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M]))))

  empirical_max_age <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_violin(ggplot2::aes(x = mainland_ex,
                                      y = max_age_ratio_empirical_means),
                         fill = "#E69F00",
                         colour = "#E69F00",
                         alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::ylab("Mean Empirical Max Age Percent (%)") +
    ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M]))))

  cowplot::plot_grid(ideal_max_age, empirical_max_age)
}

