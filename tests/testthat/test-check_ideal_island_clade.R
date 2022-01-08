test_that("empty island", {
  skip("this can be deleted as it checks the old output of sim_island")
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_clade = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  ideal_island_clade <- island$ideal_island[[1]]
  expect_silent(check_ideal_island_clade(ideal_island_clade))
})

test_that("island with two clades", {
  skip("this can be deleted as it checks the old output of sim_island")
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  ideal_island_clade <- island$ideal_island[[1]]
  expect_silent(check_ideal_island_clade(ideal_island_clade))
})

test_that("island with two clades, detailed", {
  skip("this can be deleted as it checks the old output of sim_island")
  # 1. Create a nice clade
  # 2. Invalidate a copy of that clade
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  ideal_island_clade <- island$ideal_island[[1]]
  expect_silent(check_ideal_island_clade(ideal_island_clade))

  # branching_times
  bad_ideal_island_clade <- ideal_island_clade
  bad_ideal_island_clade$branching_times <- "nonsense"
  expect_error(check_ideal_island_clade(bad_ideal_island_clade))

  # stac
  bad_ideal_island_clade <- ideal_island_clade
  bad_ideal_island_clade$stac <- "nonsense"
  expect_error(check_ideal_island_clade(bad_ideal_island_clade))

  # missing_species
  bad_ideal_island_clade <- ideal_island_clade
  bad_ideal_island_clade$missing_species <- "nonsense"
  expect_error(check_ideal_island_clade(bad_ideal_island_clade))
})

test_that("island with two clades and colonisations", {
  skip("this can be deleted as it checks the old output of sim_island")
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
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  ideal_island_clade <- island$ideal_island[[2]]
  check_ideal_island_clade(ideal_island_clade)
})
