#' Plots two faceted boxplots of the percent of endemics on the island for
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

  endemic_list <- lapply(error_list, "[[", "endemic_percent")
  endemic_empirical <- lapply(endemic_list,
                              "[[",
                              "empirical_endemic_percent")
  endemic_ideal <- lapply(endemic_list,
                          "[[",
                          "ideal_endemic_percent")

  mainland_ex <- unlist(lapply(sim_params_list, "[[", "mainland_ex"))
  mainland_sample_prob <- unlist(lapply(sim_params_list, "[[",
                                        "mainland_sample_prob"))
  mainland_sample_type <- unlist(lapply(sim_params_list, "[[",
                                        "mainland_sample_type"))

  mainland_ex_list <- list()
  mainland_sample_prob_list <- list()
  mainland_sample_type_list <- list()
  for (i in seq_along(endemic_empirical)) {
    mainland_ex_list[[i]] <- rep(
      mainland_ex[i],
      length(endemic_empirical[[i]]))
    mainland_sample_prob_list[[i]] <- rep(
      mainland_sample_prob[i],
      length(endemic_empirical[[i]]))
    mainland_sample_type_list[[i]] <- rep(
      mainland_sample_type[i],
      length(endemic_empirical[[i]]))
  }

  plotting_data <- data.frame(
    endemic_empirical = unlist(endemic_empirical),
    endemic_ideal = unlist(endemic_ideal),
    mainland_ex = unlist(mainland_ex_list),
    mainland_sample_prob = unlist(mainland_sample_prob_list),
    mainland_sample_type = unlist(mainland_sample_type))

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
    ideal_endemics <- ggplot2::ggplot(data = plotting_data,
                                       ggplot2::aes(
                                         x = as.factor(mainland_ex),
                                         y = endemic_ideal)) +
      ggplot2::stat_summary(fun.data = calc_quantiles,
                            geom = "boxplot",
                            fill = "#009E73",
                            lwd = 0.5) +
      ggplot2::stat_summary(fun = calc_outliers,
                            geom = "point",
                            size = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab("Ideal Endemic %") +
      ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M])))) +
      ggplot2::theme(text = ggplot2::element_text(size = 7.5)) +
      ggplot2::ylim(c(0, 100))

    empirical_endemics <- ggplot2::ggplot(data = plotting_data,
                                          ggplot2::aes(
                                            x = as.factor(mainland_ex),
                                            y = endemic_empirical)) +
      ggplot2::stat_summary(fun.data = calc_quantiles,
                            geom = "boxplot",
                            fill = "#E69F00",
                            lwd = 0.5) +
      ggplot2::stat_summary(fun = calc_outliers,
                            geom = "point",
                            size = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab("Empirical Endemic %") +
      ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M])))) +
      ggplot2::theme(text = ggplot2::element_text(size = 7.5)) +
      ggplot2::ylim(c(0, 100))
  } else {
    ideal_endemics <- ggplot2::ggplot(data = plotting_data,
                                       ggplot2::aes(
                                         x = as.factor(mainland_sample_prob),
                                         y = endemic_ideal)) +
      ggplot2::stat_summary(fun.data = calc_quantiles,
                            geom = "boxplot",
                            fill = "#009E73",
                            lwd = 0.5) +
      ggplot2::stat_summary(fun = calc_outliers,
                            geom = "point",
                            size = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab("Ideal Endemic %") +
      ggplot2::xlab(expression(paste("Mainland sampling probability ",
                                     (rho)))) +
      ggplot2::theme(text = ggplot2::element_text(size = 7.5)) +
      ggplot2::ylim(c(0, 100))

    empirical_endemics <- ggplot2::ggplot(data = plotting_data,
                                          ggplot2::aes(
                                            x = as.factor(mainland_sample_prob),
                                            y = endemic_empirical)) +
      ggplot2::stat_summary(fun.data = calc_quantiles,
                            geom = "boxplot",
                            fill = "#E69F00",
                            lwd = 0.5) +
      ggplot2::stat_summary(fun = calc_outliers,
                            geom = "point",
                            size = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::ylab("Empirical Endemic %") +
      ggplot2::xlab(expression(paste("Mainland sampling probability ",
                                     (rho)))) +
      ggplot2::theme(text = ggplot2::element_text(size = 7.5)) +
      ggplot2::ylim(c(0, 100))
  }

  endemics <- cowplot::plot_grid(ideal_endemics, empirical_endemics,
                                 labels = c("C", "D"),
                                 label_size = 10)

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
