test_that("stac == 6", {
  # Endemic clade with unknown colonisation time, but with a maximum to this
  # colonisation time
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_scenario <- sample(1:22, size = 1)
  expect_equal(mainland_scenario, 21)
  mainland_clade <- create_test_mainland_clade(
    mainland_scenario = mainland_scenario
  )
  plot_mainland_clade(mainland_clade)
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  plot_island(island)
})
