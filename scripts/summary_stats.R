# Produce summary statistic

analysis_results <- DAISIEmainland::read_analysis_results(
  data_folder_path = file.path("results")
)

DAISIEmainland::calc_sim_metrics(
  analysis_results = analysis_results,
  output_file_path = NULL)
