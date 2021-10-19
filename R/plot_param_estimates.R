#' Plots a grid of density plots of parameter estimates on the diagonal and
#' scatter plots of parameter estimates under the diagonal.
#'
#' @inheritParams default_params_doc
#'
#' @return Void (saves plot)
#' @export
plot_param_estimates <- function(param_set,
                                 data_folder_path,
                                 output_file_path) {

  param_space_name <- paste0("param_set_", param_set, ".rds")
  files <- list.files(data_folder_path, pattern = param_space_name)

  if (length(files) == 0) {
    stop("No results are in the results directory")
  } else {
    file_paths <- as.list(paste0(data_folder_path, "/", files))
    results_list <- lapply(file_paths, readRDS)
  }

  ideal_ml <- results_list[[1]]$ideal_ml
  ideal_clado <- unlist(lapply(ideal_ml, "[[", "lambda_c"))
  ideal_ext <- unlist(lapply(ideal_ml, "[[", "mu"))
  ideal_k <- unlist(lapply(ideal_ml, "[[", "K"))
  ideal_immig <- unlist(lapply(ideal_ml, "[[", "gamma"))
  ideal_ana <- unlist(lapply(ideal_ml, "[[", "lambda_a"))

  ideal_clado <- asinh(ideal_clado)
  ideal_ext <- asinh(ideal_ext)
  ideal_k <- asinh(ideal_k)
  ideal_immig <- asinh(ideal_immig)
  ideal_ana <- asinh(ideal_ana)

  empirical_ml <- results_list[[1]]$empirical_ml
  empirical_clado <- unlist(lapply(empirical_ml, "[[", "lambda_c"))
  empirical_ext <- unlist(lapply(empirical_ml, "[[", "mu"))
  empirical_k <- unlist(lapply(empirical_ml, "[[", "K"))
  empirical_immig <- unlist(lapply(empirical_ml, "[[", "gamma"))
  empirical_ana <- unlist(lapply(empirical_ml, "[[", "lambda_a"))

  empirical_clado <- asinh(empirical_clado)
  empirical_ext <- asinh(empirical_ext)
  empirical_k <- asinh(empirical_k)
  empirical_immig <- asinh(empirical_immig)
  empirical_ana <- asinh(empirical_ana)

  param_diffs_list <- results_list[[1]]$error$param_diffs
  clado_diffs <- param_diffs_list$clado_diff
  ext_diffs <- param_diffs_list$ext_diff
  k_diffs <- param_diffs_list$k_diff
  immig_diffs <- param_diffs_list$immig_diff
  ana_diffs <- param_diffs_list$ana_diff

  clado_diffs <- asinh(clado_diffs)
  ext_diffs <- asinh(ext_diffs)
  k_diffs <- asinh(k_diffs)
  immig_diffs <- asinh(immig_diffs)
  ana_diffs <- asinh(ana_diffs)

  sim_params <- results_list[[1]]$sim_params
  sim_clado <- asinh(sim_params$island_clado)
  sim_ext <- asinh(sim_params$island_ex)
  sim_k <- asinh(sim_params$island_k)
  sim_immig <- asinh(sim_params$island_immig)
  sim_ana <- asinh(sim_params$island_ana)

  plotting_data <- data.frame(ideal_clado = ideal_clado,
                              ideal_ext = ideal_ext,
                              ideal_k = ideal_k,
                              ideal_immig = ideal_immig,
                              ideal_ana = ideal_ana,
                              empirical_clado = empirical_clado,
                              empirical_ext = empirical_ext,
                              empirical_k = empirical_k,
                              empirical_immig = empirical_immig,
                              empirical_ana = empirical_ana)

  clado_density <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = ideal_clado),
                          fill = "#009E73",
                          colour = "#009E73",
                          alpha = 0.3) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = empirical_clado),
                          fill = "#E69F00",
                          colour = "#E69F00",
                          alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(tilde(lambda^c))) +
    ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  ext_density <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = ideal_ext),
                          fill = "#009E73",
                          colour = "#009E73",
                          alpha = 0.3) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = empirical_ext),
                          fill = "#E69F00",
                          colour = "#E69F00",
                          alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(tilde(mu))) +
    ggplot2::geom_vline(xintercept = sim_ext, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  immig_density <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = ideal_immig),
                          fill = "#009E73",
                          colour = "#009E73",
                          alpha = 0.3) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = empirical_immig),
                          fill = "#E69F00",
                          colour = "#E69F00",
                          alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(tilde(gamma))) +
    ggplot2::geom_vline(xintercept = sim_immig, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  ana_density <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = ideal_ana),
                          fill = "#009E73",
                          colour = "#009E73",
                          alpha = 0.3) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = empirical_ana),
                          fill = "#E69F00",
                          colour = "#E69F00",
                          alpha = 0.3) +
    ggplot2::theme_classic() +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(tilde(lambda^a))) +
    ggplot2::geom_vline(xintercept = sim_ana, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  ext_vs_clado <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_clado,
                                               y = ideal_ext),
                        colour = "#009E73",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = empirical_clado,
                                               y = empirical_ext),
                        colour = "#E69F00",
                        shape = 17,
                        alpha = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = sim_clado,
                                               y = sim_ext),
                        shape = 15) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab(expression(tilde(mu))) +
    ggplot2::xlab(expression(tilde(lambda^c))) +
    ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_ext, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  immig_vs_clado <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_clado,
                                               y = ideal_immig),
                        colour = "#009E73",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = empirical_clado,
                                               y = empirical_immig),
                        colour = "#E69F00",
                        shape = 17,
                        alpha = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = sim_clado,
                                               y = sim_immig),
                        shape = 15) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab(expression(tilde(gamma))) +
    ggplot2::xlab(expression(tilde(lambda^c))) +
    ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_immig, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  ana_vs_clado <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_clado,
                                               y = ideal_ana),
                        colour = "#009E73",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = empirical_clado,
                                               y = empirical_ana),
                        colour = "#E69F00",
                        shape = 17,
                        alpha = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = sim_clado,
                                               y = sim_ana),
                        shape = 15) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab(expression(tilde(lambda^a))) +
    ggplot2::xlab(expression(tilde(lambda^c))) +
    ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_ana, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  immig_vs_ext <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_ext,
                                               y = ideal_immig),
                        colour = "#009E73",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = empirical_ext,
                                               y = empirical_immig),
                        colour = "#E69F00",
                        shape = 17,
                        alpha = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = sim_ext,
                                               y = sim_immig),
                        shape = 15) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab(expression(tilde(gamma))) +
    ggplot2::xlab(expression(tilde(mu))) +
    ggplot2::geom_vline(xintercept = sim_ext, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_immig, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  ana_vs_ext <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_ext,
                                               y = ideal_ana),
                        colour = "#009E73",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = empirical_ext,
                                               y = empirical_ana),
                        colour = "#E69F00",
                        shape = 17,
                        alpha = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = sim_ext,
                                               y = sim_ana),
                        shape = 15) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab(expression(tilde(lambda^a))) +
    ggplot2::xlab(expression(tilde(mu))) +
    ggplot2::geom_vline(xintercept = sim_ext, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_ana, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  ana_vs_immig <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_immig,
                                               y = ideal_ana),
                        colour = "#009E73",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = empirical_immig,
                                               y = empirical_ana),
                        colour = "#E69F00",
                        shape = 17,
                        alpha = 0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = sim_immig,
                                               y = sim_ana),
                        shape = 15) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab(expression(tilde(lambda^a))) +
    ggplot2::xlab(expression(tilde(gamma))) +
    ggplot2::geom_vline(xintercept = sim_immig, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_ana, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  clado_vs_ext_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ext_diffs,
                                               y = clado_diffs),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::ylab(expression(tilde(paste(Delta, lambda^c)))) +
    ggplot2::xlab(expression(tilde(paste(Delta, mu)))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  clado_vs_immig_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = immig_diffs,
                                               y = clado_diffs),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::ylab(expression(tilde(paste(Delta, lambda^c)))) +
    ggplot2::xlab(expression(tilde(paste(Delta, gamma)))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  clado_vs_ana_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ana_diffs,
                                               y = clado_diffs),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::ylab(expression(tilde(paste(Delta, lambda^c)))) +
    ggplot2::xlab(expression(tilde(paste(Delta, lambda^a)))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  ext_vs_immig_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = immig_diffs,
                                               y = ext_diffs),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::ylab(expression(tilde(paste(Delta, mu)))) +
    ggplot2::xlab(expression(tilde(paste(Delta, gamma)))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  ext_vs_ana_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ana_diffs,
                                               y = ext_diffs),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::ylab(expression(tilde(paste(Delta, mu)))) +
    ggplot2::xlab(expression(tilde(paste(Delta, lambda^a)))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  immig_vs_ana_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ana_diffs,
                                               y = immig_diffs),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::ylab(expression(tilde(paste(Delta, gamma)))) +
    ggplot2::xlab(expression(tilde(paste(Delta, lambda^a)))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::theme(text = ggplot2::element_text(size = 7.5))

  param_estimates <- cowplot::plot_grid(
    clado_density, clado_vs_ext_diffs, clado_vs_immig_diffs, clado_vs_ana_diffs,
    ext_vs_clado, ext_density, ext_vs_immig_diffs, ext_vs_ana_diffs,
    immig_vs_clado, immig_vs_ext, immig_density, immig_vs_ana_diffs,
    ana_vs_clado, ana_vs_ext, ana_vs_immig, ana_density)

  if (!is.null(output_file_path)) {
    ggplot2::ggsave(
      plot = param_estimates,
      filename = output_file_path,
      device = "png",
      width = 168,
      height = 100,
      units = "mm",
      dpi = 600
    )
  } else {
    return(param_estimates)
  }
}
