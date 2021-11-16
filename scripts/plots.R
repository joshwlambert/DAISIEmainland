# Produce the plots for the paper

DAISIEmainland::plot_param_estimates(
  param_set = 1,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_1.png"),
parameter = "mainland_ex",
num_breaks = 5,
signif = 3
)

DAISIEmainland::plot_param_estimates(
  param_set = 3,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_3.png"),
parameter = "mainland_ex",
num_breaks = 5,
signif = 2
)

DAISIEmainland::plot_param_estimates(
  param_set = 21,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_21.png"),
parameter = "mainland_ex",
num_breaks = 5,
signif = 2
)

DAISIEmainland::plot_param_estimates(
  param_set = 35,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_35.png"),
parameter = "undiscovered",
num_breaks = 5,
signif = 2
)

DAISIEmainland::plot_param_estimates(
  param_set = 43,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_43.png"),
parameter = "undiscovered",
num_breaks = 5,
signif = 2
)

DAISIEmainland::plot_param_estimates(
  param_set = 23,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_23.png"),
parameter = "unsampled",
num_breaks = 5,
signif = 2
)

DAISIEmainland::plot_param_estimates(
  param_set = 31,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_31.png"),
parameter = "unsampled",
num_breaks = 5,
signif = 2
)

DAISIEmainland::plot_k_estimates(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "k_estimates_mainland_ex.png"),
  parameter = "mainland_ex",
  num_breaks = 4,
  signif = 2)

DAISIEmainland::plot_k_estimates(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "k_estimates_unsampled.png"),
  parameter = "unsampled",
  num_breaks = 4,
  signif = 2)

DAISIEmainland::plot_k_estimates(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "k_estimates_undiscovered.png"),
  parameter = "undiscovered",
  num_breaks = 4,
  signif = 2)
