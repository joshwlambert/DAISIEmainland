#' Plots carrying capacity estimates
#'
#' @inheritParams default_params_doc
#'
#' @return Void (saves plot)
#' @export
plot_k_estimates <- function(data_folder_path,
                             output_file_path,
                             param_space) {

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
  sim_k <- unlist(lapply(sim_params_list, "[[", "island_k"))
  mainland_ex <- unlist(lapply(sim_params_list, "[[", 6))
  mainland_sample_prob <- unlist(lapply(sim_params_list, "[[", 7))

  param_diffs_list <- lapply(error_list, "[[", "param_diffs")
  k_diffs_list <- lapply(param_diffs_list, "[[", "k_diff")

  mean_k_diffs_no_inf <- unlist(lapply(k_diffs_list, function(x) {
    temp_x <- x[!is.infinite(x)]
    temp_x <- temp_x[!is.nan(temp_x)]
    mean(temp_x)
  }))

  median_k_diffs_no_inf <- unlist(lapply(k_diffs_list, function(x) {
    temp_x <- x[!is.infinite(x)]
    temp_x <- temp_x[!is.nan(temp_x)]
    stats::median(temp_x)
  }))

  plotting_data <- data.frame(percent_ideal_k_inf = percent_ideal_k_inf,
                              percent_empirical_k_inf = percent_empirical_k_inf,
                              sim_k = sim_k,
                              mainland_ex = mainland_ex,
                              mainland_sample_prob = mainland_sample_prob,
                              mean_k_diffs_no_inf = mean_k_diffs_no_inf,
                              median_k_diffs_no_inf = median_k_diffs_no_inf)

  mean_k_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(ggplot2::aes(x = mainland_ex,
                                     y = mean_k_diffs_no_inf)) +
    ggplot2::theme_classic() +
    ggplot2::facet_wrap(ggplot2::vars(sim_k)) +
    ggplot2::ylab(expression(bar(paste(Delta, "K'")))) +
    ggplot2::xlab(expression(mu[M]))

  median_k_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(ggplot2::aes(x = mainland_ex,
                                     y = median_k_diffs_no_inf)) +
    ggplot2::theme_classic() +
    ggplot2::facet_wrap(ggplot2::vars(sim_k)) +
    ggplot2::ylim(c(-20, 20)) +
    ggplot2::ylab(expression(tilde(paste(Delta, "K'")))) +
    ggplot2::xlab(expression(mu[M]))

  median_k_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_violin(ggplot2::aes(x = factor(mainland_ex),
                                      y = median_k_diffs_no_inf)) +
    ggplot2::theme_classic() +
    ggplot2::facet_wrap(ggplot2::vars(sim_k)) +
    ggplot2::ylim(c(-20, 20)) +
    ggplot2::ylab(expression(tilde(paste(Delta, "K'")))) +
    ggplot2::xlab(expression(mu[M]))


  percent_k_inf <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(ggplot2::aes(x = mainland_ex,
                                     y = percent_ideal_k_inf)) +
    ggplot2::geom_point(ggplot2::aes(x = mainland_ex,
                                     y = percent_empirical_k_inf),
                        shape = 15) +
    ggplot2::facet_wrap(ggplot2::vars(sim_k)) +
    ggplot2::ylab("Percentage of Infinite Ideal K' (%)") +


  percent_k_inf <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_bar(ggplot2::aes(y = percent_ideal_k_inf))

  k_estimates <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_tile(ggplot2::aes(x = mainland_ex,
                                    y = mainland_sample_prob,
                                    fill = ctt_means)) +
    ggplot2::theme_classic() +
    ggplot2::ylab(expression(paste("Mainland sampling probability ", (rho)))) +
    ggplot2::xlab(expression(paste("Mainland extinction ", (mu[M])))) +
    ggplot2::scale_fill_continuous(name = expression(paste(Delta, "CTT"))) +
    ggplot2::scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5))

  ggplot2::ggsave(
    plot = ctt,
    filename = file.path("plots", "ctt"),
    device = "png",
    width = 168,
    height = 100,
    units = "mm",
    dpi = 600
  )
}
