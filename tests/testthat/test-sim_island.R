test_that("sim_island is silent and produces correct empty island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland <- create_test_mainland(mainland_scenario = 1)
  expect_silent(
    island <- sim_island(
      time = 1,
      m = 10,
      island_pars = c(1, 1, 10, 1, 1),
      mainland = mainland[[1]],
      mainland_sample_prob = 1)
  )
  expect_equal(island$ideal_island$branching_times, 1)
  expect_equal(island$ideal_island$stac, 0)
  expect_equal(island$ideal_island$missing_species, 0)
  expect_equal(island$empirical_island$branching_times, 1)
  expect_equal(island$empirical_island$stac, 0)
  expect_equal(island$empirical_island$missing_species, 0)
})

test_that("sim_island is silent and produces correct non-empty island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland <- create_test_mainland(mainland_scenario = 2)
  expect_silent(
    island <- sim_island(
      time = 1,
      m = 100,
      island_pars = c(1, 1, 10, 1, 1),
      mainland = mainland[[1]],
      mainland_sample_prob = 1)
  )
  expect_equal(island$ideal_island[[1]]$branching_times,
               c(1.000000000000, 0.312294050817, 0.191759806927))
  expect_equal(island$ideal_island[[1]]$stac, 2)
  expect_equal(island$ideal_island[[1]]$missing_species, 0)
  expect_equal(island$empirical_island[[1]]$branching_times,
               c(1.000000000000, 0.312294050817, 0.191759806927))
  expect_equal(island$empirical_island[[1]]$stac, 2)
  expect_equal(island$empirical_island[[1]]$missing_species, 0)
})
