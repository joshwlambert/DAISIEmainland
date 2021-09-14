#' Plots two faceted violin plots of the percent of endemics on the island for
#' different values of mainland extinction. The left side facet has the ideal
#' data and the right side facet has the empirical data.
#'
#' @return Plot
#' @export
plot_endemics <- function() {

  files <- list.files(file.path(getwd(), "results"))

  if (length(files) == 0) {
    stop("No results are in the results directory")
  } else {
    file_paths <- as.list(paste0(file.path(getwd(), "results"), "/", files))
    results_list <- lapply(file_paths, readRDS)
  }

  error_list <- lapply(results_list, '[[', "error")
  sim_params_list <- lapply(results_list, '[[', "sim_params")

  endemic_percent_list <- lapply(error_list, '[[', "endemic_percent")
  endemic_percent_empirical_list <- lapply(endemic_percent_list,
                                           '[[',
                                           "empirical_endemic_percent")
  endemic_percent_ideal_list <- lapply(endemic_percent_list,
                                       '[[',
                                       "ideal_endemic_percent")

  endemic_percent_empirical_means <- unlist(lapply(
    endemic_percent_empirical_list,
    mean))
  endemic_percent_ideal_means <- unlist(lapply(
    endemic_percent_ideal_list,
    mean))


  mainland_ex <- unlist(lapply(sim_params_list, '[[', 6))
  mainland_sample_prob <- unlist(lapply(sim_params_list, '[[', 7))

  plotting_data <- data.frame(
    endemic_percent_empirical_means = endemic_percent_empirical_means,
    endemic_percent_ideal_means = endemic_percent_ideal_means,
    mainland_ex = as.factor(mainland_ex),
    mainland_sample_prob = as.factor(mainland_sample_prob))

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
    ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M]))))

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
    ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M]))))

  endemics <- cowplot::plot_grid(ideal_endemics, empirical_endemics)
  return(endemics)
}

