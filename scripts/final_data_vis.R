# Final data visualisatio to produce the plots for the paper

DAISIEmainland::plot_sim_metrics(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "sim_metrics.png"))

ctt_mainland_ex <- DAISIEmainland::plot_ctt_boxplot(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  parameter = "mainland_ex")

ctt_unsampled <- DAISIEmainland::plot_ctt_boxplot(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  parameter = "unsampled")

ctt <- cowplot::plot_grid(
  ctt_mainland_ex,
  ctt_unsampled,
  nrow = 1,
  labels = c("A", "B"),
  label_size = 10)

ggplot2::ggsave(
  plot = ctt,
  filename = file.path("plots", "ctt.png"),
  device = "png",
  width = 160,
  height = 80,
  units = "mm",
  dpi = 600
)

max_age <- DAISIEmainland::plot_max_age(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  parameter = "mainland_ex")

endemics <- DAISIEmainland::plot_endemics(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  parameter = "mainland_ex")

max_age_and_endemics <- cowplot::plot_grid(max_age, endemics, ncol = 1)

ggplot2::ggsave(
  plot = max_age_and_endemics,
  filename = file.path("plots", "max_age_and_endemics_mainland_ex.png"),
  device = "png",
  width = 180,
  height = 80,
  units = "mm",
  dpi = 600
)

max_age <- DAISIEmainland::plot_max_age(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  parameter = "unsampled")

endemics <- DAISIEmainland::plot_endemics(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  parameter = "unsampled")

max_age_and_endemics <- cowplot::plot_grid(max_age, endemics, ncol = 1)

ggplot2::ggsave(
  plot = max_age_and_endemics,
  filename = file.path("plots", "max_age_and_endemics_unsampled.png"),
  device = "png",
  width = 180,
  height = 80,
  units = "mm",
  dpi = 600
)

max_age <- DAISIEmainland::plot_max_age(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  parameter = "undiscovered")

endemics <- DAISIEmainland::plot_endemics(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  parameter = "undiscovered")

max_age_and_endemics <- cowplot::plot_grid(max_age, endemics, ncol = 1)

ggplot2::ggsave(
  plot = max_age_and_endemics,
  filename = file.path("plots", "max_age_and_endemics_undiscovered.png"),
  device = "png",
  width = 180,
  height = 80,
  units = "mm",
  dpi = 600
)

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

DAISIEmainland::plot_inf_k(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "inf_k.png"))
