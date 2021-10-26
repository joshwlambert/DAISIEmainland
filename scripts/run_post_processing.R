DAISIEmainland::calc_overall_sim_metrics(
  data_folder_path = file.path("results"),
  output_file_path = NULL)

DAISIEmainland::plot_sim_metrics(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "sim_metrics.png"))

DAISIEmainland::plot_ctt_boxplot(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "mainland_ex_ctt_scatter.png"),
  parameter = "mainland_ex")

DAISIEmainland::plot_ctt_boxplot(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "unsampled_ctt_scatter.png"),
  parameter = "unsampled")

DAISIEmainland::plot_ctt_boxplot(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "undiscovered_ctt_scatter.png"),
  parameter = "undiscovered")

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
  width = 168,
  height = 100,
  units = "mm",
  dpi = 600
)

data("param_space")
for (i in seq_along(list.files(file.path("results")))) {

  if (param_space$mainland_sample_type[i] == "complete") {
    parameter <- "mainland_ex"
  } else if (param_space$mainland_sample_type[i] == "unsampled") {
    parameter <- "unsampled"
  } else if (param_space$mainland_sample_type[i] == "undiscovered") {
    parameter <- "undiscovered"
  }

  DAISIEmainland::plot_param_estimates(
    param_set = i,
    data_folder_path = file.path("results"),
    output_file_path = file.path("plots",
                                 paste0("param_estimates_", i, ".png")),
    parameter = parameter
  )
}

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
  width = 168,
  height = 100,
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
  width = 168,
  height = 100,
  units = "mm",
  dpi = 600
)

DAISIEmainland::plot_k_estimates(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "k_estimates_mainland_ex.png"),
  parameter = "mainland_ex")

DAISIEmainland::plot_k_estimates(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "k_estimates_unsampled.png"),
  parameter = "unsampled")

DAISIEmainland::plot_k_estimates(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "k_estimates_undiscovered.png"),
  parameter = "undiscovered")

DAISIEmainland::plot_inf_k(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "inf_k.png"))
