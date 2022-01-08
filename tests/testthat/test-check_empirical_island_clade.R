test_that("sim_island is silent and produces correct empty island", {
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
  empirical_island_clade <- island$empirical_island[[1]]
  expect_silent(check_empirical_island_clade(empirical_island_clade))
})

test_that("sim_island is silent and produces correct non-empty island", {
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
  empirical_island_clade <- island$empirical_island[[1]]
  expect_silent(check_empirical_island_clade(empirical_island_clade))
})
