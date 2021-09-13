DAISIEmainland::plot_param_estimates(
  sim_params = c(param_space$island_clado[args],
                 param_space$island_ex[args],
                 param_space$island_k[args],
                 param_space$island_immig[args],
                 param_space$island_ana[args]),
  ideal_ml = ideal_ml,
  empirical_ml = empirical_ml,
  param_set = args,
  xlim = FALSE)

DAISIEmainland::plot_param_estimates(
  sim_params = c(param_space$island_clado[args],
                 param_space$island_ex[args],
                 param_space$island_k[args],
                 param_space$island_immig[args],
                 param_space$island_ana[args]),
  ideal_ml = ideal_ml,
  empirical_ml = empirical_ml,
  param_set = args,
  xlim = TRUE)

DAISIEmainland::plot_ctt()

#plot max age

#plot endemic

#plot carrying capacity
