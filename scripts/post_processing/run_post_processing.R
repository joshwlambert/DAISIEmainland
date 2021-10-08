DAISIEmainland::calc_overall_sim_metrics(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  param_space = "general")

DAISIEmainland::plot_sim_metrics(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "sim_metrics.png"))

DAISIEmainland::plot_ctt_heatmap(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "ctt_heatmap.png"))

DAISIEmainland::plot_ctt_scatter(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "mainland_ex_ctt_scatter.png"),
  parameter = "mainland_ex")

DAISIEmainland::plot_ctt_scatter(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "mainland_sample_prob_scatter.png"),
  parameter = "mainland_sample_prob")

ctt_mainland_ex <- DAISIEmainland::plot_ctt_scatter(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  parameter = "mainland_ex")

ctt_mainland_sample_prob <- DAISIEmainland::plot_ctt_scatter(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  parameter = "mainland_sample_prob")

ctt_heatmap <- DAISIEmainland::plot_ctt_heatmap(
  data_folder_path = file.path("results"),
  output_file_path = NULL)

ctt_scatter <- cowplot::plot_grid(
  ctt_mainland_ex,
  ctt_mainland_sample_prob,
  nrow = 1, labels = "AUTO")

ctt <- cowplot::plot_grid(
  ctt_scatter,
  ctt_heatmap,
  nrow = 2,
  labels = c('', "C"))

ggplot2::ggsave(
  plot = ctt,
  filename = file.path("plots", "ctt.png"),
  device = "png",
  width = 168,
  height = 100,
  units = "mm",
  dpi = 600
)

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

max_age <- DAISIEmainland::plot_max_age(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  parameter = "both")

endemics <- DAISIEmainland::plot_endemics(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  parameter = "both")

max_age_and_endemics <- cowplot::plot_grid(max_age, endemics, ncol = 1)

ggplot2::ggsave(
  plot = max_age_and_endemics,
  filename = file.path("plots", "max_age_and_endemics_all.png"),
  device = "png",
  width = 168,
  height = 100,
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
  width = 168,
  height = 100,
  units = "mm",
  dpi = 600
)

max_age <- DAISIEmainland::plot_max_age(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  parameter = "mainland_sample_prob")

endemics <- DAISIEmainland::plot_endemics(
  data_folder_path = file.path("results"),
  output_file_path = NULL,
  parameter = "mainland_sample_prob")

max_age_and_endemics <- cowplot::plot_grid(max_age, endemics, ncol = 1)

ggplot2::ggsave(
  plot = max_age_and_endemics,
  filename = file.path("plots",
                       "max_age_and_endemics_mainland_sample_prob.png"),
  device = "png",
  width = 168,
  height = 100,
  units = "mm",
  dpi = 600
)

#plot carrying capacity
