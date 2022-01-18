# Final data visualisation to produce the plots for the paper

analysis_results <- DAISIEmainland::read_analysis_results(
  data_folder_path = file.path("results")
)

DAISIEmainland::plot_sim_metrics(
  analysis_results = analysis_results,
  output_file_path = file.path("plots", "sim_metrics.png")
)

ctt_mainland_ex <- DAISIEmainland::plot_ctt_boxplot(
  analysis_results = analysis_results,
  output_file_path = NULL,
  parameter = "mainland_ex"
)

ctt_unsampled <- DAISIEmainland::plot_ctt_boxplot(
  analysis_results = analysis_results,
  output_file_path = NULL,
  parameter = "unsampled"
)

ctt <- cowplot::plot_grid(
  ctt_mainland_ex,
  ctt_unsampled,
  nrow = 1,
  labels = c("A", "B"),
  label_size = 10
)

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
  analysis_results = analysis_results,
  output_file_path = NULL,
  parameter = "mainland_ex",
  labels = c("A", "B")
)

endemics <- DAISIEmainland::plot_endemics(
  analysis_results = analysis_results,
  output_file_path = NULL,
  parameter = "mainland_ex",
  labels = c("C", "D")
)

max_age_and_endemics <- cowplot::plot_grid(max_age, endemics, ncol = 1)

ggplot2::ggsave(
  plot = max_age_and_endemics,
  filename = file.path("plots", "max_age_and_endemics_mainland_ex.png"),
  device = "png",
  width = 180,
  height = 120,
  units = "mm",
  dpi = 600
)

max_age <- DAISIEmainland::plot_max_age(
  analysis_results = analysis_results,
  output_file_path = NULL,
  parameter = "unsampled",
  labels = c("A", "B")
)

endemics <- DAISIEmainland::plot_endemics(
  analysis_results = analysis_results,
  output_file_path = NULL,
  parameter = "unsampled",
  labels = c("C", "D")
)

max_age_and_endemics <- cowplot::plot_grid(max_age, endemics, ncol = 1)

ggplot2::ggsave(
  plot = max_age_and_endemics,
  filename = file.path("plots", "max_age_and_endemics_unsampled.png"),
  device = "png",
  width = 180,
  height = 120,
  units = "mm",
  dpi = 600
)

max_age <- DAISIEmainland::plot_max_age(
  analysis_results = analysis_results,
  output_file_path = NULL,
  parameter = "undiscovered",
  labels = c("A", "B")
)

endemics <- DAISIEmainland::plot_endemics(
  analysis_results = analysis_results,
  output_file_path = NULL,
  parameter = "undiscovered",
  labels = c("C", "D")
)

max_age_and_endemics <- cowplot::plot_grid(max_age, endemics, ncol = 1)

ggplot2::ggsave(
  plot = max_age_and_endemics,
  filename = file.path("plots", "max_age_and_endemics_undiscovered.png"),
  device = "png",
  width = 180,
  height = 120,
  units = "mm",
  dpi = 600
)

DAISIEmainland::plot_param_diffs(
  param_set = 1,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_1.png"),
  parameter = "mainland_ex",
  signif = 3,
  scientific = FALSE,
  transform = "ihs"
)

DAISIEmainland::plot_param_diffs(
  param_set = 3,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_3.png"),
  parameter = "mainland_ex",
  signif = 2,
  scientific = FALSE,
  transform = "ihs"
)

DAISIEmainland::plot_param_diffs(
  param_set = 21,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_21.png"),
  parameter = "mainland_ex",
  signif = 2,
  scientific = FALSE,
  transform = "ihs"
)

DAISIEmainland::plot_param_diffs(
  param_set = 35,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_35.png"),
  parameter = "undiscovered",
  signif = 2,
  scientific = FALSE,
  transform = "ihs"
)

DAISIEmainland::plot_param_diffs(
  param_set = 43,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_43.png"),
  parameter = "undiscovered",
  signif = 2,
  scientific = FALSE,
  transform = "ihs"
)

DAISIEmainland::plot_param_diffs(
  param_set = 23,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_23.png"),
  parameter = "unsampled",
  signif = 2,
  scientific = FALSE,
  transform = "ihs"
)

DAISIEmainland::plot_param_diffs(
  param_set = 31,
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "param_estimates_31.png"),
  parameter = "unsampled",
  signif = 2,
  scientific = FALSE,
  transform = "ihs"
)

DAISIEmainland::plot_k_estimates(
  analysis_results = analysis_results,
  output_file_path = file.path("plots", "k_estimates_mainland_ex.png"),
  parameter = "mainland_ex",
  num_breaks = 4,
  signif = 2,
  scientific = TRUE,
  labels = c("", "", "A", "B", "", "", "C", "D")
)

DAISIEmainland::plot_k_estimates(
  analysis_results = analysis_results,
  output_file_path = file.path("plots", "k_estimates_unsampled.png"),
  parameter = "unsampled",
  num_breaks = 4,
  signif = 2,
  scientific = TRUE,
  labels = c("", "", "A", "B", "", "", "C", "D")
)

DAISIEmainland::plot_k_estimates(
  analysis_results = analysis_results,
  output_file_path = file.path("plots", "k_estimates_undiscovered.png"),
  parameter = "undiscovered",
  num_breaks = 4,
  signif = 2,
  scientific = TRUE,
  labels = c("", "", "A", "B", "", "", "C", "D")
)

DAISIEmainland::plot_inf_k(
  analysis_results = analysis_results,
  output_file_path = file.path("plots", "inf_k.png"),
  labels = c("A", "B")
)
