# test_that("use", {
#   set.seed(
#     2,
#     kind = "Mersenne-Twister",
#     normal.kind = "Inversion",
#     sample.kind = "Rejection"
#   )
#   mainland_clade <- create_test_mainland_clade(
#     mainland_scenario = 20
#   )
#   plot_mainland_clade(mainland_clade)
#   island_tbl <- sim_island(
#     total_time = 1,
#     island_pars = c(1, 1, 10, 12, 1),
#     mainland = mainland_clade,
#     mainland_sample_prob = 1,
#     mainland_sample_type = "complete"
#   )
#   daisie_data <- create_daisie_data(
#     total_time = 1,
#     island_tbl = island_tbl,
#     mainland_clade = mainland_clade,
#     mainland_sample_prob = 1,
#     mainland_sample_type = "complete"
#   )
#   t <- all_colonisations_to_table(
#     ideal_or_empirical_island = daisie_data$ideal_island
#   )
# })
#
# test_that("stress-test", {
#   for (seed in seq_len(1)) {
#     set.seed(
#       seed,
#       kind = "Mersenne-Twister",
#       normal.kind = "Inversion",
#       sample.kind = "Rejection"
#     )
#     mainland_scenario <- sample(x = seq(1, 22), size = 1)
#     mainland_clade <- create_test_mainland_clade(
#       mainland_scenario = mainland_scenario
#     )
#     island_tbl <- sim_island(
#       total_time = 1,
#       island_pars = c(1, 1, 10, 12, 1),
#       mainland = mainland_clade,
#       mainland_sample_prob = 1,
#       mainland_sample_type = "complete"
#     )
#     daisie_data <- create_daisie_data(
#       total_time = 1,
#       island_tbl = island_tbl,
#       mainland_clade = mainland_clade,
#       mainland_sample_prob = 1,
#       mainland_sample_type = "complete"
#     )
#     all_colonisations_to_table(
#       ideal_or_empirical_island = daisie_data$ideal_island
#     )
#   }
# })
