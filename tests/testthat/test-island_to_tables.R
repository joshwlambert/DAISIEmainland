test_that("use", {
  mainland_clade <- create_test_mainland_clade(
    mainland_scenario = 21
  )
  plot_mainland_clade(mainland_clade)
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  island_to_tables(island)
})
