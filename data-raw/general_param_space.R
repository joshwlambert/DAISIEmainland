## code to prepare `general_param_space` dataset goes here
general_param_space <- expand.grid(
  total_time = c(5),
  m = c(1000),
  island_clado = c(0.5),
  island_ex = c(0.25),
  island_k = c(5, 50),
  island_immig = c(0.01),
  island_ana = c(0.5),
  mainland_ex = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0),
  mainland_sample_prob = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
  replicates = c(500))
usethis::use_data(general_param_space, overwrite = TRUE)
