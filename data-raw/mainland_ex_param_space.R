## code to prepare `mainland_ex_param_space` dataset goes here
mainland_ex_param_space <- expand.grid(
  total_time = c(5),
  m = c(500),
  island_clado = c(0.5),
  island_ex = c(0.5),
  island_k = c(10),
  island_immig = c(0.05),
  island_ana = c(0.5),
  mainland_ex = seq(0, 5, 0.1),
  mainland_sample_prob = c(1.0),
  replicates = c(100))
usethis::use_data(mainland_ex_param_space, overwrite = TRUE)
