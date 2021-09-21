data("param_space")

for (param_set in 1:nrow(param_space)) {
  DAISIEmainland::plot_param_estimates(
    sim_params = c(param_space$island_clado[param_set],
                   param_space$island_ex[param_set],
                   param_space$island_k[param_set],
                   param_space$island_immig[param_set],
                   param_space$island_ana[param_set]),
    ideal_ml = ideal_ml,
    empirical_ml = empirical_ml,
    param_set = param_set,
    xlim = FALSE,
    data_folder_path = file.path("results"),
    output_file_path = file.path("plots",
                                 paste0(plot_title,
                                        "_param_estimates.png",
                                        param_set)))

  DAISIEmainland::plot_param_estimates(
    sim_params = c(param_space$island_clado[param_set],
                   param_space$island_ex[param_set],
                   param_space$island_k[param_set],
                   param_space$island_immig[param_set],
                   param_space$island_ana[param_set]),
    ideal_ml = ideal_ml,
    empirical_ml = empirical_ml,
    param_set = param_set,
    xlim = TRUE)

  #could list all the results in a list and then lapply mean and var

  mean(output$ideal_sim_metrics$num_col)
  var(output$ideal_sim_metrics$num_col)
  max(output$ideal_sim_metrics$num_col)
  min(output$ideal_sim_metrics$num_col)
  mean(output$ideal_sim_metrics$num_spec)
  var(output$ideal_sim_metrics$num_spec)
  max(output$ideal_sim_metrics$num_spec)
  min(output$ideal_sim_metrics$num_spec)
  mean(output$empirical_sim_metrics$num_col)
  var(output$empirical_sim_metrics$num_col)
  max(output$empirical_sim_metrics$num_col)
  min(output$empirical_sim_metrics$num_col)
  mean(output$empirical_sim_metrics$num_spec)
  var(output$empirical_sim_metrics$num_spec)
  max(output$empirical_sim_metrics$num_spec)
  min(output$empirical_sim_metrics$num_spec)
}

DAISIEmainland::plot_ctt()

max_age <- DAISIEmainland::plot_max_age()

endemics <- DAISIEmainland::plot_endemics()

max_age_and_endemics <- cowplot::plot_grid(max_age, endemics, ncol = 1)

ggplot2::ggsave(
  plot = max_age_and_endemics,
  filename = file.path("plots", "max_age_and_endemics.png"),
  device = "png",
  width = 168,
  height = 100,
  units = "mm",
  dpi = 600
)

ideal_results <- calc_sim_metrics(daisie_data = island$ideal_islands)
empirical_results <- calc_sim_metrics(daisie_data = island$empirical_islands)

#plot carrying capacity
