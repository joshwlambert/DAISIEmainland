test_that("use", {
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  # Pick an interesting one
  mainland_clade <- create_test_mainland_clade(
    mainland_scenario = 20
  )
  plot_mainland_clade(mainland_clade)
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  plot_island(island = island)
  t <- all_colonisations_to_table(ideal_or_empirical_island = island$ideal_island)

})
