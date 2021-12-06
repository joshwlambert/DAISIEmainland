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
  empirical_island <- island$empirical_island
  stacs <- collect_empirical_island_stacs(empirical_island)
  expect_true(any(stacs == 6))
  plot_empirical_island(empirical_island = empirical_island)
})

test_that("stac == 5", {
  set.seed(
    48,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_scenario <- sample(1:22, size = 1)
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
  empirical_island <- island$empirical_island
  stacs <- collect_empirical_island_stacs(empirical_island)
  expect_equal(5, stacs)
  plot_empirical_island(empirical_island = empirical_island)
})

test_that("stac == 1", {
  set.seed(
    113,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_scenario <- sample(1:22, size = 1)
  mainland_clade <- create_test_mainland_clade(
    mainland_scenario = mainland_scenario
  )
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  empirical_island <- island$empirical_island
  stacs <- collect_empirical_island_stacs(empirical_island)
  expect_equal(1, stacs)
  plot_empirical_island(empirical_island = empirical_island)
})

test_that("find stacs in emperical data", {
  # Helper test to detect interesting cases
  # Test more heavy if you want :-)
  for (seed in seq_len(1)) {
    message("seed: ", seed)
    set.seed(
      seed,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
    mainland_scenario <- sample(1:22, size = 1)
    message("mainland_scenario: ", mainland_scenario)
    mainland_clade <- create_test_mainland_clade(
      mainland_scenario = mainland_scenario
    )
    island <- sim_island(
      total_time = 1,
      island_pars = c(1, 1, 10, 12, 1),
      mainland = mainland_clade,
      mainland_sample_prob = 1,
      mainland_sample_type = "complete")
    empirical_island <- island$empirical_island
    stacs <- collect_empirical_island_stacs(empirical_island)
    if (any(stacs %in% c(1))) {
      message("FOUND")
      message(seed)
    }
  }
})
