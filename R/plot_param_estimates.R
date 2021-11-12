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
                                 parameter) {

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

  empirical_ml <- results_list[[1]]$empirical_ml
  empirical_clado <- unlist(lapply(empirical_ml, "[[", "lambda_c"))
  empirical_ext <- unlist(lapply(empirical_ml, "[[", "mu"))
  empirical_immig <- unlist(lapply(empirical_ml, "[[", "gamma"))
  empirical_ana <- unlist(lapply(empirical_ml, "[[", "lambda_a"))

  param_ratios_list <- results_list[[1]]$error$param_ratios
  clado_ratios <- param_ratios_list$clado_ratios
  ext_ratios <- param_ratios_list$ext_ratios
  immig_ratios <- param_ratios_list$immig_ratios
  ana_ratios <- param_ratios_list$ana_ratios

  sim_params <- results_list[[1]]$sim_params
  sim_clado <- sim_params$island_clado
  sim_ext <- sim_params$island_ex
  sim_immig <- sim_params$island_immig
  sim_ana <- sim_params$island_ana

  upper_clado <- max(ideal_clado, empirical_clado)
  upper_ext <- max(ideal_ext, empirical_ext)
  upper_immig <- max(ideal_immig, empirical_immig)
  upper_ana <- max(ideal_ana, empirical_ana)
  lower_clado <- min(ideal_clado, empirical_clado)
  lower_ext <- min(ideal_ext, empirical_ext)
  lower_immig <- min(ideal_immig, empirical_immig)
  lower_ana <- min(ideal_ana, empirical_ana)
  upper_clado_ratios <- max(clado_ratios)
  upper_ext_ratios <- max(ext_ratios)
  upper_immig_ratios <- max(immig_ratios)
  upper_ana_ratios <- max(ana_ratios)
  lower_clado_ratios <- min(clado_ratios)
  lower_ext_ratios <- min(ext_ratios)
  lower_immig_ratios <- min(immig_ratios)
  lower_ana_ratios <- min(ana_ratios)

  plotting_data <- data.frame(ideal_clado = ideal_clado,
                              ideal_ext = ideal_ext,
                              ideal_immig = ideal_immig,
                              ideal_ana = ideal_ana,
                              empirical_clado = empirical_clado,
                              empirical_ext = empirical_ext,
                              empirical_immig = empirical_immig,
                              empirical_ana = empirical_ana,
                              clado_ratios = clado_ratios,
                              ext_ratios = ext_ratios,
                              immig_ratios = immig_ratios,
                              ana_ratios = ana_ratios)

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
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda^c)) +
    ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50") +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_clado,
                                  upper_lim = upper_clado,
                                  accuracy = 0.1,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_clado,
                                  upper_lim = upper_clado,
                                  accuracy = 0.1,
                                  round_func = floor),
      limits = c(lower_clado, upper_clado),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

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
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(mu)) +
    ggplot2::geom_vline(xintercept = sim_ext, colour = "grey50") +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_ext,
                                  upper_lim = upper_ext,
                                  accuracy = 0.1,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_ext,
                                  upper_lim = upper_ext,
                                  accuracy = 0.1,
                                  round_func = floor),
      limits = c(lower_ext, upper_ext),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

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
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(gamma)) +
    ggplot2::geom_vline(xintercept = sim_immig, colour = "grey50") +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_immig,
                                  upper_lim = upper_immig,
                                  accuracy = 0.001,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_immig,
                                  upper_lim = upper_immig,
                                  accuracy = 0.001,
                                  round_func = floor),
      limits = c(lower_immig, upper_immig),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

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
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab("Density") +
    ggplot2::xlab(expression(lambda^a)) +
    ggplot2::geom_vline(xintercept = sim_ana, colour = "grey50") +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_ana,
                                  upper_lim = upper_ana,
                                  accuracy = 0.1,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_ana,
                                  upper_lim = upper_ana,
                                  accuracy = 0.1,
                                  round_func = floor),
      limits = c(lower_ana, upper_ana),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

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
    ggplot2::theme(legend.position = "none",
                   title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab(expression(mu)) +
    ggplot2::xlab(expression(lambda^c)) +
    ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_ext, colour = "grey50") +
    ggplot2::scale_y_continuous(
      breaks = create_plot_breaks(lower_lim = lower_ext,
                                  upper_lim = upper_ext,
                                  accuracy = 0.1,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_ext,
                                  upper_lim = upper_ext,
                                  accuracy = 0.1,
                                  round_func = floor),
      limits = c(lower_ext, upper_ext),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh)) +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_clado,
                                  upper_lim = upper_clado,
                                  accuracy = 0.1,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_clado,
                                  upper_lim = upper_clado,
                                  accuracy = 0.1,
                                  round_func = floor),
      limits = c(lower_clado, upper_clado),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

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
    ggplot2::theme(legend.position = "none",
                   title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab(expression(gamma)) +
    ggplot2::xlab(expression(lambda^c)) +
    ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_immig, colour = "grey50") +
    ggplot2::scale_y_continuous(
      breaks = create_plot_breaks(lower_lim = lower_immig,
                                  upper_lim = upper_immig,
                                  accuracy = 0.001,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_immig,
                                  upper_lim = upper_immig,
                                  accuracy = 0.001,
                                  round_func = floor),
      limits = c(lower_immig, upper_immig),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh)) +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_clado,
                                  upper_lim = upper_clado,
                                  accuracy = 0.1,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_clado,
                                  upper_lim = upper_clado,
                                  accuracy = 0.1,
                                  round_func = floor),
      limits = c(lower_clado, upper_clado),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

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
    ggplot2::theme(legend.position = "none",
                   title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab(expression(lambda^a)) +
    ggplot2::xlab(expression(lambda^c)) +
    ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_ana, colour = "grey50") +
    ggplot2::scale_y_continuous(
      breaks = create_plot_breaks(lower_lim = lower_ana,
                                  upper_lim = upper_ana,
                                  accuracy = 0.1,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_ana,
                                  upper_lim = upper_ana,
                                  accuracy = 0.1,
                                  round_func = floor),
      limits = c(lower_ana, upper_ana),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh)) +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_clado,
                                  upper_lim = upper_clado,
                                  accuracy = 0.1,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_clado,
                                  upper_lim = upper_clado,
                                  accuracy = 0.1,
                                  round_func = floor),
      limits = c(lower_clado, upper_clado),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

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
    ggplot2::theme(legend.position = "none",
                   title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab(expression(gamma)) +
    ggplot2::xlab(expression(mu)) +
    ggplot2::geom_vline(xintercept = sim_ext, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_immig, colour = "grey50") +
    ggplot2::scale_y_continuous(
      breaks = create_plot_breaks(lower_lim = lower_immig,
                                  upper_lim = upper_immig,
                                  accuracy = 0.001,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_immig,
                                  upper_lim = upper_immig,
                                  accuracy = 0.001,
                                  round_func = floor),
      limits = c(lower_immig, upper_immig),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh)) +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_ext,
                                  upper_lim = upper_ext,
                                  accuracy = 0.1,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_ext,
                                  upper_lim = upper_ext,
                                  accuracy = 0.1,
                                  round_func = floor),
      limits = c(lower_ext, upper_ext),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

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
    ggplot2::theme(legend.position = "none",
                   title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab(expression(lambda^a)) +
    ggplot2::xlab(expression(mu)) +
    ggplot2::geom_vline(xintercept = sim_ext, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_ana, colour = "grey50") +
    ggplot2::scale_y_continuous(
      breaks = create_plot_breaks(lower_lim = lower_ana,
                                  upper_lim = upper_ana,
                                  accuracy = 0.1,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_ana,
                                  upper_lim = upper_ana,
                                  accuracy = 0.1,
                                  round_func = floor),
      limits = c(lower_ana, upper_ana),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh)) +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_ext,
                                  upper_lim = upper_ext,
                                  accuracy = 0.1,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_ext,
                                  upper_lim = upper_ext,
                                  accuracy = 0.1,
                                  round_func = floor),
      limits = c(lower_ext, upper_ext),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

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
    ggplot2::theme(legend.position = "none",
                   title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab(expression(lambda^a)) +
    ggplot2::xlab(expression(gamma)) +
    ggplot2::geom_vline(xintercept = sim_immig, colour = "grey50") +
    ggplot2::geom_hline(yintercept = sim_ana, colour = "grey50") +
    ggplot2::scale_y_continuous(
      breaks = create_plot_breaks(lower_lim = lower_ana,
                                  upper_lim = upper_ana,
                                  accuracy = 0.1,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_ana,
                                  upper_lim = upper_ana,
                                  accuracy = 0.1,
                                  round_func = floor),
      limits = c(lower_ana, upper_ana),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh)) +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_immig,
                                  upper_lim = upper_immig,
                                  accuracy = 0.001,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_immig,
                                  upper_lim = upper_immig,
                                  accuracy = 0.001,
                                  round_func = floor),
      limits = c(lower_immig, upper_immig),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

  clado_vs_ext_ratios <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ext_ratios,
                                               y = clado_ratios),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab(expression(paste(Delta, lambda^c))) +
    ggplot2::xlab(expression(paste(Delta, mu))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::scale_y_continuous(
      breaks = create_plot_breaks(lower_lim = lower_clado_ratios,
                                  upper_lim = upper_clado_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_clado_ratios,
                                  upper_lim = upper_clado_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      limits = c(lower_clado_ratios, upper_clado_ratios),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh)) +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_ext_ratios,
                                  upper_lim = upper_ext_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_ext_ratios,
                                  upper_lim = upper_ext_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      limits = c(lower_ext_ratios, upper_ext_ratios),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

  clado_vs_immig_ratios <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = immig_ratios,
                                               y = clado_ratios),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab(expression(paste(Delta, lambda^c))) +
    ggplot2::xlab(expression(paste(Delta, gamma))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::scale_y_continuous(
      breaks = create_plot_breaks(lower_lim = lower_clado_ratios,
                                  upper_lim = upper_clado_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_clado_ratios,
                                  upper_lim = upper_clado_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      limits = c(lower_clado_ratios, upper_clado_ratios),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh)) +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_immig_ratios,
                                  upper_lim = upper_immig_ratios,
                                  accuracy = 0.001,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_immig_ratios,
                                  upper_lim = upper_immig_ratios,
                                  accuracy = 0.001,
                                  round_func = floor),
      limits = c(lower_immig_ratios, upper_immig_ratios),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

  clado_vs_ana_ratios <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ana_ratios,
                                               y = clado_ratios),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab(expression(paste(Delta, lambda^c))) +
    ggplot2::xlab(expression(paste(Delta, lambda^a))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::scale_y_continuous(
      breaks = create_plot_breaks(lower_lim = lower_clado_ratios,
                                  upper_lim = upper_clado_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_clado_ratios,
                                  upper_lim = upper_clado_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      limits = c(lower_clado_ratios, upper_clado_ratios),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh)) +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_ana_ratios,
                                  upper_lim = upper_ana_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_ana_ratios,
                                  upper_lim = upper_ana_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      limits = c(lower_ana_ratios, upper_ana_ratios),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

  ext_vs_immig_ratios <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = immig_ratios,
                                               y = ext_ratios),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab(expression(paste(Delta, mu))) +
    ggplot2::xlab(expression(paste(Delta, gamma))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::scale_y_continuous(
      breaks = create_plot_breaks(lower_lim = lower_ext_ratios,
                                  upper_lim = upper_ext_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_ext_ratios,
                                  upper_lim = upper_ext_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      limits = c(lower_ext_ratios, upper_ext_ratios),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh)) +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_immig_ratios,
                                  upper_lim = upper_immig_ratios,
                                  accuracy = 0.001,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_immig_ratios,
                                  upper_lim = upper_immig_ratios,
                                  accuracy = 0.001,
                                  round_func = floor),
      limits = c(lower_immig_ratios, upper_immig_ratios),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

  ext_vs_ana_ratios <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ana_ratios,
                                               y = ext_ratios),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab(expression(paste(Delta, mu))) +
    ggplot2::xlab(expression(paste(Delta, lambda^a))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::scale_y_continuous(
      breaks = create_plot_breaks(lower_lim = lower_ext_ratios,
                                  upper_lim = upper_ext_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_ext_ratios,
                                  upper_lim = upper_ext_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      limits = c(lower_ext_ratios, upper_ext_ratios),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh)) +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_ana_ratios,
                                  upper_lim = upper_ana_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_ana_ratios,
                                  upper_lim = upper_ana_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      limits = c(lower_ana_ratios, upper_ana_ratios),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

  immig_vs_ana_ratios <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ana_ratios,
                                               y = immig_ratios),
                        colour = "#56B4E9",
                        shape = 16,
                        alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::theme(title = ggplot2::element_text(size = 10),
                   text = ggplot2::element_text(size = 7),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown()) +
    ggplot2::ylab(expression(paste(Delta, gamma))) +
    ggplot2::xlab(expression(paste(Delta, lambda^a))) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50") +
    ggplot2::scale_y_continuous(
      breaks = create_plot_breaks(lower_lim = lower_immig_ratios,
                                  upper_lim = upper_immig_ratios,
                                  accuracy = 0.001,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_immig_ratios,
                                  upper_lim = upper_immig_ratios,
                                  accuracy = 0.001,
                                  round_func = floor),
      limits = c(lower_immig_ratios, upper_immig_ratios),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh)) +
    ggplot2::scale_x_continuous(
      breaks = create_plot_breaks(lower_lim = lower_ana_ratios,
                                  upper_lim = upper_ana_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      labels = create_plot_labels(lower_lim = lower_ana_ratios,
                                  upper_lim = upper_ana_ratios,
                                  accuracy = 0.01,
                                  round_func = floor),
      limits = c(lower_ana_ratios, upper_ana_ratios),
      trans = scales::trans_new(name = "ihs",
                                transform = asinh,
                                inverse = sinh))

  param_estimates <- cowplot::plot_grid(
    clado_density, clado_vs_ext_ratios,
    clado_vs_immig_ratios, clado_vs_ana_ratios,
    ext_vs_clado, ext_density,
    ext_vs_immig_ratios, ext_vs_ana_ratios,
    immig_vs_clado, immig_vs_ext,
    immig_density, immig_vs_ana_ratios,
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
      width = 168,
      height = 100,
      units = "mm",
      dpi = 600
    )
  } else {
    return(param_estimates)
  }
}
