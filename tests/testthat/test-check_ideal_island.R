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
  expect_silent(check_ideal_island(island$ideal_island))
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
  ideal_island <- island$ideal_island
  expect_silent(check_ideal_island(ideal_island))
})

test_that("detect an invalid clade", {
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
  ideal_island <- island$ideal_island

  # We need two clades, as we invalidate the second one
  expect_equal(length(ideal_island), 2)
  ideal_island[[2]]$branching_times <- "nonsense"

  expect_error(check_ideal_island(ideal_island))
})
