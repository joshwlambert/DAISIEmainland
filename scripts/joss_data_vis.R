# Simulate mainland and island data and visualise it

set.seed(
  1,
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)

mainland <- DAISIEmainland::sim_mainland(
  total_time = 1,
  m = 5,
  mainland_ex = 1
)

DAISIEmainland::plot_mainland(
  mainland = mainland,
  branch_colour = "unique_species_id"
)

DAISIEmainland::plot_mainland(
  mainland = mainland,
  branch_colour = "clade_id"
)

set.seed(
  1,
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)

daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
  total_time = 1,
  m = 10,
  island_pars = c(1.0, 0, 10, 0.75, 0.5),
  mainland_ex = 3,
  mainland_sample_prob = 1,
  mainland_sample_type = "complete",
  replicates = 1,
  verbose = FALSE
)

DAISIEmainland::plot_daisie_mainland_data(
  daisie_mainland_data = daisie_mainland_data,
  replicate_index = 1
)

set.seed(
  1,
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)

daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
  total_time = 1,
  m = 100,
  island_pars = c(1, 1, 50, 0.01, 1),
  mainland_ex = 1,
  mainland_sample_prob = 1,
  mainland_sample_type = "complete",
  replicates = 500,
  verbose = TRUE
)

# change plotting figures to read results directly
DAISIEmainland::plot_endemics(
  analysis_results = analysis_results,
  output_file_path = NULL,
  parameter = "mainland_ex"
)
