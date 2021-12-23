#' Plots a grid of density plots of parameter estimates on the diagonal and
#' scatter plots of parameter estimates under the diagonal.
#'
#' @inheritParams default_params_doc
#'
#' @return Void (saves plot)
#' @export
plot_param_estimates <- function(param_set,
                                 data_folder_path,
                                 output_file_path,
                                 parameter,
                                 num_breaks,
                                 signif) {

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
  ideal_immig <- unlist(lapply(ideal_ml, "[[", "gamma"))
  ideal_ana <- unlist(lapply(ideal_ml, "[[", "lambda_a"))

  testit::assert(ideal_clado > 0)
  testit::assert(ideal_ext > 0)
  testit::assert(ideal_immig > 0)
  testit::assert(ideal_ana > 0)

  empirical_ml <- results_list[[1]]$empirical_ml
  empirical_clado <- unlist(lapply(empirical_ml, "[[", "lambda_c"))
  empirical_ext <- unlist(lapply(empirical_ml, "[[", "mu"))
  empirical_immig <- unlist(lapply(empirical_ml, "[[", "gamma"))
  empirical_ana <- unlist(lapply(empirical_ml, "[[", "lambda_a"))

  testit::assert(empirical_clado > 0)
  testit::assert(empirical_ext > 0)
  testit::assert(empirical_immig > 0)
  testit::assert(empirical_ana > 0)

  param_diffs_list <- results_list[[1]]$error$param_diffs
  clado_diffs <- param_diffs_list$clado_diffs
  ext_diffs <- param_diffs_list$ext_diffs
  immig_diffs <- param_diffs_list$immig_diffs
  ana_diffs <- param_diffs_list$ana_diffs

  sim_params <- results_list[[1]]$sim_params
  sim_clado <- sim_params$island_clado
  sim_ext <- sim_params$island_ex
  sim_immig <- sim_params$island_immig
  sim_ana <- sim_params$island_ana

  plotting_data <- data.frame(ideal_clado = ideal_clado,
                              ideal_ext = ideal_ext,
                              ideal_immig = ideal_immig,
                              ideal_ana = ideal_ana,
                              empirical_clado = empirical_clado,
                              empirical_ext = empirical_ext,
                              empirical_immig = empirical_immig,
                              empirical_ana = empirical_ana,
                              clado_diffs = clado_diffs,
                              ext_diffs = ext_diffs,
                              immig_diffs = immig_diffs,
                              ana_diffs = ana_diffs)

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
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda^c)) +
    ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50")

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
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(mu)) +
    ggplot2::geom_vline(xintercept = sim_ext, colour = "grey50")

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
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(gamma)) +
    ggplot2::geom_vline(xintercept = sim_immig, colour = "grey50")

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
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda^a)) +
    ggplot2::geom_vline(xintercept = sim_ana, colour = "grey50")

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
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(mu)) +
    ggplot2::xlab(expression(lambda^c)) +
    ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_ext, colour = "grey50")

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
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(gamma)) +
    ggplot2::xlab(expression(lambda^c)) +
    ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_immig, colour = "grey50")

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
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(lambda^a)) +
    ggplot2::xlab(expression(lambda^c)) +
    ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_ana, colour = "grey50")

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
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(gamma)) +
    ggplot2::xlab(expression(mu)) +
    ggplot2::geom_vline(xintercept = sim_ext, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_immig, colour = "grey50")

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
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(lambda^a)) +
    ggplot2::xlab(expression(mu)) +
    ggplot2::geom_vline(xintercept = sim_ext, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_ana, colour = "grey50")

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
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(lambda^a)) +
    ggplot2::xlab(expression(gamma)) +
    ggplot2::geom_vline(xintercept = sim_immig, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_ana, colour = "grey50")

  clado_vs_ext_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ext_diffs,
                                               y = clado_diffs),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(paste(Delta, lambda^c))) +
    ggplot2::xlab(expression(paste(Delta, mu))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50")

  clado_vs_immig_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = immig_diffs,
                                               y = clado_diffs),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(paste(Delta, lambda^c))) +
    ggplot2::xlab(expression(paste(Delta, gamma))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50")

  clado_vs_ana_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ana_diffs,
                                               y = clado_diffs),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(paste(Delta, lambda^c))) +
    ggplot2::xlab(expression(paste(Delta, lambda^a))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50")

  ext_vs_immig_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = immig_diffs,
                                               y = ext_diffs),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(paste(Delta, mu))) +
    ggplot2::xlab(expression(paste(Delta, gamma))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50")

  ext_vs_ana_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ana_diffs,
                                               y = ext_diffs),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(paste(Delta, mu))) +
    ggplot2::xlab(expression(paste(Delta, lambda^a))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50")

  immig_vs_ana_diffs <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ana_diffs,
                                               y = immig_diffs),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7)) +
    ggplot2::ylab(expression(paste(Delta, gamma))) +
    ggplot2::xlab(expression(paste(Delta, lambda^a))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50")

  param_estimates <- cowplot::plot_grid(
    clado_density, clado_vs_ext_diffs,
    clado_vs_immig_diffs, clado_vs_ana_diffs,
    ext_vs_clado, ext_density,
    ext_vs_immig_diffs, ext_vs_ana_diffs,
    immig_vs_clado, immig_vs_ext,
    immig_density, immig_vs_ana_diffs,
    ana_vs_clado, ana_vs_ext, ana_vs_immig, ana_density,
    align = "hv", nrow = 4, ncol = 4)

  if (parameter == "mainland_ex") {
    title <- cowplot::ggdraw() +
      cowplot::draw_label(
        paste0("Mainland extinction rate = ", sim_params$mainland_ex),
        size = 12)
  } else {
    title <- cowplot::ggdraw() +
      cowplot::draw_label(
        paste0("Mainland sampling probability = ",
               sim_params$mainland_sample_prob),
        size = 12)
  }
  param_estimates <- cowplot::plot_grid(
    title,
    param_estimates,
    nrow = 2, rel_heights = c(0.05, 1))


  if (!is.null(output_file_path)) {
    ggplot2::ggsave(
      plot = param_estimates,
      filename = output_file_path,
      device = "png",
      width = 180,
      height = 180,
      units = "mm",
      dpi = 600
    )
  } else {
    return(param_estimates)
  }
}
