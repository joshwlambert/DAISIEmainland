plot_param_estimates <- function(sim_params,
                                 ideal_ml,
                                 empirical_ml,
                                 xlim = FALSE) {

  ideal_clado <- unlist(lapply(ideal_ml, '[[', 1))
  ideal_ext <- unlist(lapply(ideal_ml, '[[', 2))
  ideal_k <- unlist(lapply(ideal_ml, '[[', 3))
  ideal_immig <- unlist(lapply(ideal_ml, '[[', 4))
  ideal_ana <- unlist(lapply(ideal_ml, '[[', 5))

  empirical_clado <- unlist(lapply(empirical_ml, '[[', 1))
  empirical_ext <- unlist(lapply(empirical_ml, '[[', 2))
  empirical_k <- unlist(lapply(empirical_ml, '[[', 3))
  empirical_immig <- unlist(lapply(empirical_ml, '[[', 4))
  empirical_ana <- unlist(lapply(empirical_ml, '[[', 5))

  sim_clado <- sim_params[1]
  sim_ext <- sim_params[2]
  sim_k <- sim_params[3]
  sim_immig <- sim_params[4]
  sim_ana <- sim_params[5]

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

  if (xlim) {

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
      ggplot2::xlab(expression(lambda^c)) +
      ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50") +
      ggplot2::xlim(c(0, 5))


    ext_density <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = ideal_ext),
                            fill = "cornsilk") +
      ggplot2::theme_classic() +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(mu)) +
      ggplot2::geom_vline(xintercept = sim_ext, colour = "grey50") +
      ggplot2::xlim(c(0, 5))

    immig_density <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = ideal_immig),
                            fill = "cornsilk") +
      ggplot2::theme_classic() +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(gamma)) +
      ggplot2::geom_vline(xintercept = sim_immig, colour = "grey50") +
      ggplot2::xlim(c(0, 0.5))

    ana_density <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_density(mapping = ggplot2::aes(x = ideal_ana),
                            fill = "cornsilk") +
      ggplot2::theme_classic() +
      ggplot2::ylab("Density") +
      ggplot2::xlab(expression(lambda^a)) +
      ggplot2::geom_vline(xintercept = sim_ana, colour = "grey50") +
      ggplot2::xlim(c(0, 5))

    ext_vs_clado <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_clado,
                                                 y = ideal_ext),
                          colour = "#009E73", shape = 16) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = empirical_clado,
                                                 y = empirical_ext),
                          colour = "#E69F00", shape = 17) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = sim_clado,
                                                 y = sim_ext), shape = 15) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylab(expression(mu)) +
      ggplot2::xlab(expression(lambda^c)) +
      ggplot2::ylim(c(0, 5)) +
      ggplot2::xlim(c(0, 5)) +
      ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50") +
      ggplot2::geom_hline(yintercept = sim_ext, colour = "grey50")

    immig_vs_clado <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_clado,
                                                 y = ideal_immig),
                          colour = "wheat3") +
      ggplot2::geom_point(mapping = ggplot2::aes(x = sim_clado,
                                                 y = sim_immig)) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylab(expression(gamma)) +
      ggplot2::xlab(expression(lambda^c)) +
      ggplot2::ylim(c(0, 0.5)) +
      ggplot2::xlim(c(0, 5))

    ana_vs_clado <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_clado,
                                                 y = ideal_ana),
                          colour = "wheat3") +
      ggplot2::geom_point(mapping = ggplot2::aes(x = sim_clado,
                                                 y = sim_ana)) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylab(expression(lambda^a)) +
      ggplot2::xlab(expression(lambda^c)) +
      ggplot2::ylim(c(0, 5)) +
      ggplot2::xlim(c(0, 5))

    immig_vs_ext <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_ext,
                                                 y = ideal_immig),
                          colour = "wheat3") +
      ggplot2::geom_point(mapping = ggplot2::aes(x = sim_ext,
                                                 y = sim_immig)) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylab(expression(gamma)) +
      ggplot2::xlab(expression(mu)) +
      ggplot2::ylim(c(0, 0.5)) +
      ggplot2::xlim(c(0, 5))

    ana_vs_ext <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_ext,
                                                 y = ideal_ana),
                          colour = "wheat3") +
      ggplot2::geom_point(mapping = ggplot2::aes(x = sim_ext,
                                                 y = sim_ana)) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylab(expression(lambda^a)) +
      ggplot2::xlab(expression(mu)) +
      ggplot2::ylim(c(0, 5)) +
      ggplot2::xlim(c(0, 5))

    ana_vs_immig <- ggplot2::ggplot(data = plotting_data) +
      ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_immig,
                                                 y = ideal_ana),
                          colour = "wheat3") +
      ggplot2::geom_point(mapping = ggplot2::aes(x = sim_immig,
                                                 y = sim_ana)) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylab(expression(lambda^a)) +
      ggplot2::xlab(expression(gamma)) +
      ggplot2::ylim(c(0, 5)) +
      ggplot2::xlim(c(0, 0.5))

    } else {

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
        ggplot2::xlab(expression(lambda^c)) +
        ggplot2::geom_vline(xintercept = sim_clado, colour = "grey50")

      ext_density <- ggplot2::ggplot(data = plotting_data) +
        ggplot2::geom_density(mapping = ggplot2::aes(x = ideal_ext),
                              fill = "cornsilk") +
        ggplot2::theme_classic() +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(mu)) +
        ggplot2::geom_vline(xintercept = sim_ext, colour = "grey50")

      immig_density <- ggplot2::ggplot(data = plotting_data) +
        ggplot2::geom_density(mapping = ggplot2::aes(x = ideal_immig),
                              fill = "cornsilk") +
        ggplot2::theme_classic() +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(gamma)) +
        ggplot2::geom_vline(xintercept = sim_immig, colour = "grey50")

      ana_density <- ggplot2::ggplot(data = plotting_data) +
        ggplot2::geom_density(mapping = ggplot2::aes(x = ideal_ana),
                              fill = "cornsilk") +
        ggplot2::theme_classic() +
        ggplot2::ylab("Density") +
        ggplot2::xlab(expression(lambda^a)) +
        ggplot2::geom_vline(xintercept = sim_ana, colour = "grey50")

      ext_vs_clado <- ggplot2::ggplot(data = plotting_data) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_clado,
                                                   y = ideal_ext),
                            colour = "wheat3") +
        ggplot2::geom_point(mapping = ggplot2::aes(x = sim_clado,
                                                   y = sim_ext)) +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ylab(expression(mu)) +
        ggplot2::xlab(expression(lambda^c))

      immig_vs_clado <- ggplot2::ggplot(data = plotting_data) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_clado,
                                                   y = ideal_immig),
                            colour = "wheat3") +
        ggplot2::geom_point(mapping = ggplot2::aes(x = sim_clado,
                                                   y = sim_immig)) +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ylab(expression(gamma)) +
        ggplot2::xlab(expression(lambda^c))

      ana_vs_clado <- ggplot2::ggplot(data = plotting_data) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_clado,
                                                   y = ideal_ana),
                            colour = "wheat3") +
        ggplot2::geom_point(mapping = ggplot2::aes(x = sim_clado,
                                                   y = sim_ana)) +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ylab(expression(lambda^a)) +
        ggplot2::xlab(expression(lambda^c))

      immig_vs_ext <- ggplot2::ggplot(data = plotting_data) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_ext,
                                                   y = ideal_immig),
                            colour = "wheat3") +
        ggplot2::geom_point(mapping = ggplot2::aes(x = sim_ext,
                                                   y = sim_immig)) +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ylab(expression(gamma)) +
        ggplot2::xlab(expression(mu))

      ana_vs_ext <- ggplot2::ggplot(data = plotting_data) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_ext,
                                                   y = ideal_ana),
                            colour = "wheat3") +
        ggplot2::geom_point(mapping = ggplot2::aes(x = sim_ext,
                                                   y = sim_ana)) +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ylab(expression(lambda^a)) +
        ggplot2::xlab(expression(mu))

      ana_vs_immig <- ggplot2::ggplot(data = plotting_data) +
        ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_immig,
                                                   y = ideal_ana),
                            colour = "wheat3") +
        ggplot2::geom_point(mapping = ggplot2::aes(x = sim_immig,
                                                   y = sim_ana)) +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ylab(expression(lambda^a)) +
        ggplot2::xlab(expression(gamma))
    }

  cowplot::plot_grid(clado_density, NULL, NULL, NULL,
                     ext_vs_clado, ext_density, NULL, NULL,
                     immig_vs_clado, immig_vs_ext, immig_density, NULL,
                     ana_vs_clado, ana_vs_ext, ana_vs_immig, ana_density)
}
