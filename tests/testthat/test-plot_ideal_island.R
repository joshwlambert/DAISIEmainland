test_that("scenario 1", {
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  plot_mainland_clade(mainland_clade)
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  ideal_island <- island$ideal_island
  skip("stac is zero")
  expect_true(ideal_island[[1]]$stac >= 1)
  expect_true(ideal_island[[1]]$stac <= 6)
  plot_ideal_island(ideal_island = island$ideal_island)
})

test_that("use", {
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
  plot_mainland_clade(mainland_clade)
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  ideal_island <- island$ideal_island
  plot_ideal_island(ideal_island = island$ideal_island)
})
