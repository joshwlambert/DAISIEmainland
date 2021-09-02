plot_param_estimates <- function(sim_params,
                                 ideal_ml,
                                 empirical_ml) {

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

  clado_density <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = ideal_clado),
                          fill = "cornsilk") +
    ggplot2::theme_classic()

  ext_density <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = ideal_ext),
                          fill = "cornsilk") +
    ggplot2::theme_classic()

  immig_density <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = ideal_immig),
                          fill = "cornsilk") +
    ggplot2::theme_classic()

  ana_density <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_density(mapping = ggplot2::aes(x = ideal_ana),
                          fill = "cornsilk") +
    ggplot2::theme_classic()

  clado_vs_ext <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_clado,
                                               y = ideal_ext),
                        colour = "wheat3") +
    ggplot2::geom_point(mapping = ggplot2::aes(x = sim_clado,
                                               y = sim_ext)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none")

  clado_vs_immig <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_clado,
                                               y = ideal_immig)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = sim_clado,
                                               y = sim_immig,
                                               colour = "red")) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none")

  clado_vs_ana <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_clado,
                                               y = ideal_ana)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = sim_clado,
                                               y = sim_ana,
                                               colour = "red")) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none")

  ext_vs_immig <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_ext,
                                               y = ideal_immig)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = sim_ext,
                                               y = sim_immig,
                                               colour = "red")) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none")

  ext_vs_ana <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_ext,
                                               y = ideal_ana)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = sim_ext,
                                               y = sim_ana,
                                               colour = "red")) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none")

  immig_vs_ana <- ggplot2::ggplot(data = plotting_data) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = ideal_immig,
                                               y = ideal_ana)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = sim_immig,
                                               y = sim_ana,
                                               colour = "red")) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none")

  cowplot::plot_grid(clado_density, NULL, NULL, NULL,
                     clado_vs_ext, ext_density, NULL, NULL,
                     clado_vs_immig, ext_vs_immig, immig_density, NULL,
                     clado_vs_ana, ext_vs_ana, immig_vs_ana, ana_density)


  hist(ideal_clado,
       ylab="",
       xlab="",
       bty="n",
       main="",
       breaks=25,
       cex.axis=0.8,
       col= 'cornsilk2',
       border= 'cornsilk2')
  abline(v=median(ideal_clado), col='dark grey', lwd=2)
  abline(v=sim_clado, col="darkgreen", lwd=2)

  plot(NULL)
  plot(NULL)
  plot(NULL)

  plot(NULL,
       xlim = c(min(ideal_clado, sim_clado),
                max(ideal_clado, sim_clado)),
       ylim = c(min(ideal_ext, sim_ext),
                max(ideal_ext, sim_ext)),
       cex.axis = 0.8)
  points(ideal_clado,
         ideal_ext,
         pch=16, cex=0.8, col='cornsilk2')
  points(sim_clado, sim_ext, col='dark grey', pch=16, cex=1)


  hist(ideal_ext,
       ylab="",
       xlab="",
       bty="n",
       main="",
       breaks=25,
       cex.axis=0.8,
       col= 'cornsilk2',
       border= 'cornsilk2')
  abline(v = median(ideal_ext),col= 'dark grey',lwd=2)
  abline(v = sim_ext,col = "darkgreen", lwd = 2)

  plot(NULL)
  plot(NULL)

  plot(NULL,
       xlim = c(min(ideal_clado, sim_clado),
                max(ideal_clado, sim_clado)),
       ylim = c(min(ideal_immig, sim_immig),
                max(ideal_immig, sim_immig)),
       cex.axis = 0.8)
  points(ideal_clado,
         ideal_immig,
         pch=16, cex=0.8, col='cornsilk2')
  points(sim_clado, sim_immig, col='dark grey', pch=16, cex=1)

  plot(NULL,
       xlim = c(min(ideal_ext, sim_ext),
                max(ideal_ext, sim_ext)),
       ylim = c(min(ideal_immig, sim_immig),
                max(ideal_immig, sim_immig)),
       cex.axis = 0.8)
  points(ideal_ext,
         ideal_immig,
         pch=16, cex=0.8, col='cornsilk2')
  points(sim_ext, sim_immig, col='dark grey', pch=16, cex=1)

  hist(ideal_immig,
       ylab="",
       xlab="",
       bty="n",
       main="",
       breaks=25,
       cex.axis=0.8,
       col= 'cornsilk2',
       border= 'cornsilk2')
  abline(v=median(ideal_immig),col= 'dark grey',lwd=2)
  abline(v=sim_immig, col="darkgreen",lwd=2)

  plot(NULL)

  plot(NULL,
       xlim = c(min(ideal_clado, sim_clado),
                max(ideal_clado, sim_clado)),
       ylim = c(min(ideal_ana, sim_ana),
                max(ideal_ana, sim_ana)),
       cex.axis = 0.8)
  points(ideal_clado,
         ideal_ana,
         pch=16, cex=0.8, col='cornsilk2')
  points(sim_clado, sim_ana, col='dark grey', pch=16, cex=1)

  plot(NULL,
       xlim = c(min(ideal_ext, sim_ext),
                max(ideal_ext, sim_ext)),
       ylim = c(min(ideal_ana, sim_ana),
                max(ideal_ana, sim_ana)),
       cex.axis = 0.8)
  points(ideal_ext,
         ideal_ana,
         pch=16, cex=0.8, col='cornsilk2')
  points(sim_ext, sim_ana, col='dark grey', pch=16, cex=1)

  plot(NULL,
       xlim = c(min(ideal_immig, sim_immig),
                max(ideal_immig, sim_immig)),
       ylim = c(min(ideal_ana, sim_ana),
                max(ideal_ana, sim_ana)),
       cex.axis = 0.8)
  points(ideal_immig,
         ideal_ana,
         pch=16, cex=0.8, col='cornsilk2')
  points(sim_immig, sim_ana, col='dark grey', pch=16, cex=1)

  hist(ideal_ana,
       ylab="",
       xlab="",
       bty="n",
       main="",
       breaks=25,
       cex.axis=0.8,
       col='cornsilk2',
       border= 'cornsilk2')
  abline(v=median(ideal_ana),col= 'dark grey',lwd=2)
  abline(v=sim_ana,col="darkgreen",lwd=2)

}
