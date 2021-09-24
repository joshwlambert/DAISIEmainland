## code to prepare `mainland_sample_prob_param_space` dataset goes here
mainland_sample_prob_param_space <- expand.grid(
  total_time = c(5),
  m = c(1000),
  island_clado = c(0.5),
  island_ex = c(0.5),
  island_k = c(10),
  island_immig = c(0.01),
  island_ana = c(0.5),
  mainland_ex = c(0),
  mainland_sample_prob = seq(0, 1.0, 0.02),
  replicates = c(10))
usethis::use_data(mainland_sample_prob_param_space, overwrite = TRUE)
