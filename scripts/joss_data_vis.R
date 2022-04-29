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

mainland_species_id <- DAISIEmainland::plot_mainland(
  mainland = mainland,
  branch_colour = "unique_species_id"
)

mainland_clade_id <- DAISIEmainland::plot_mainland(
  mainland = mainland,
  branch_colour = "clade_id"
)

ggplot2::ggsave(
  plot = mainland_clade_id,
  filename = file.path("scripts/mainland_clade_id.png"),
  device = "png",
  width = 180,
  height = 100,
  units = "mm",
  dpi = 600
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

daisie_mainland_data <- DAISIEmainland::plot_daisie_mainland_data(
  daisie_mainland_data = daisie_mainland_data,
  replicate_index = 1
)

ggplot2::ggsave(
  plot = daisie_mainland_data,
  filename = file.path("scripts/daisie_mainland_data.png"),
  device = "png",
  width = 180,
  height = 100,
  units = "mm",
  dpi = 600
)
