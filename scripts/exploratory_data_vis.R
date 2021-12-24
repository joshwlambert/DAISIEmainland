# Produce a range of plots for data visualisation

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
  output_file_path = file.path("plots", "max_age_mainland_ex.png"),
  parameter = "mainland_ex")

endemics <- DAISIEmainland::plot_endemics(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "endemics_mainland_ex.png"),
  parameter = "mainland_ex")

max_age <- DAISIEmainland::plot_max_age(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "max_age_unsampled.png"),
  parameter = "unsampled")

endemics <- DAISIEmainland::plot_endemics(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "endemics_unsampled.png"),
  parameter = "unsampled")

max_age <- DAISIEmainland::plot_max_age(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "max_age_undiscovered.png"),
  parameter = "undiscovered")

endemics <- DAISIEmainland::plot_endemics(
  data_folder_path = file.path("results"),
  output_file_path = file.path("plots", "endemics_undiscovered.png"),
  parameter = "undiscovered")

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
