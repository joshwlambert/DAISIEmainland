data("param_space")

DAISIEmainland::calc_overall_sim_metrics(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  param_space = "general")

DAISIEmainland::plot_sim_metrics(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "sim_metrics.png"))

DAISIEmainland::plot_param_estimates(
  param_set = 1,
  xlim = FALSE,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_1.png"),
  param_space = "general")

DAISIEmainland::plot_param_estimates(
  param_set = 21,
  xlim = FALSE,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_21.png"),
  param_space = "general")

DAISIEmainland::plot_param_estimates(
  param_set = 111,
  xlim = FALSE,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_111.png"),
  param_space = "general")

DAISIEmainland::plot_param_estimates(
  param_set = 131,
  xlim = FALSE,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_131.png"),
  param_space = "general")

DAISIEmainland::plot_param_estimates(
  param_set = 1,
  xlim = TRUE,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_1_xlim.png"),
  param_space = "general")

DAISIEmainland::plot_param_estimates(
  param_set = 21,
  xlim = TRUE,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_21_xlim.png"),
  param_space = "general")

DAISIEmainland::plot_param_estimates(
  param_set = 111,
  xlim = TRUE,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_111_xlim.png"),
  param_space = "general")

DAISIEmainland::plot_param_estimates(
  param_set = 131,
  xlim = TRUE,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_131_xlim.png"),
  param_space = "general")


DAISIEmainland::plot_ctt_heatmap(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "ctt_heatmap.png"))

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

#plot carrying capacity
