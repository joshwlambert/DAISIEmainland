## code to prepare `param_space` dataset goes here
mainland_ex_param_space <- expand.grid(
  total_time = c(5),
  m = c(1000),
  island_clado = c(0.5),
  island_ex = c(0.25),
  island_k = c(5, 50),
  island_immig = c(0.01),
  island_ana = c(0.5),
  mainland_ex = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0),
  mainland_sample_prob = c(1.0),
  mainland_sample_type = "complete",
  replicates = c(500),
  stringsAsFactors = FALSE)
seed <- sample(x = 1:100000, size = nrow(mainland_ex_param_space))
mainland_ex_param_space <- cbind(mainland_ex_param_space, seed)

unsampled_param_space <- expand.grid(
  total_time = c(5),
  m = c(1000),
  island_clado = c(0.5),
  island_ex = c(0.25),
  island_k = c(5, 50),
  island_immig = c(0.01),
  island_ana = c(0.5),
  mainland_ex = c(0),
  mainland_sample_prob = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
  mainland_sample_type = c("unsampled"),
  replicates = c(500),
  stringsAsFactors = FALSE)
seed <- sample(x = 1:100000, size = nrow(unsampled_param_space))
unsampled_param_space <- cbind(unsampled_param_space, seed)

undiscovered_param_space <- expand.grid(
  total_time = c(5),
  m = c(1000),
  island_clado = c(0.5),
  island_ex = c(0.25),
  island_k = c(5, 50),
  island_immig = c(0.01),
  island_ana = c(0.5),
  mainland_ex = c(0),
  mainland_sample_prob = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
  mainland_sample_type = c("undiscovered"),
  replicates = c(500),
  stringsAsFactors = FALSE)
undiscovered_param_space <- cbind(undiscovered_param_space, seed)

param_space <- rbind(mainland_ex_param_space,
                     unsampled_param_space,
                     undiscovered_param_space)

usethis::use_data(param_space, overwrite = TRUE)
