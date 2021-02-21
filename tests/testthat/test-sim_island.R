context("sim_island")

test_that("sim_island is silent and produces correct empty island", {
  set.seed(1)
  # RJCB: I would love a 'create_test_mainland_params_1' function here
  # and that this function would be done such as:
  #
  # mainland <- sim_mainland(create_test_mainland_params_1())
  #
  # RJCB: where I enjoy the specificity of this test,
  # I feel that such through testing should be done on one clade.
  # Sure, there should be tests for multiple clades in the end,
  # but these will just be a vectorized version of these thorough tests.
  # Even if there is interaction between clades (is there?),
  # these can be tested later
  mainland <- sim_mainland(
    time = 1,
    m = 10,
    mainland_ext = 1)
  # RJCB: I would love a 'create_test_island_params_1' function here
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
  set.seed(1)
  # RJCB: I would love a 'create_test_mainland_params_1' function here
  mainland <- sim_mainland(
    time = 1,
    m = 100,
    mainland_ext = 1)
  # RJCB: I would love a 'create_test_mainland_params_2' function here
  expect_silent(
    island <- sim_island(
      time = 1,
      m = 100,
      island_pars = c(1, 1, 10, 1, 1),
      mainland = mainland[[1]],
      mainland_sample_prob = 1)
  )
  expect_equal(island$ideal_island[[1]]$branching_times,
               c(1.000000000000, 0.949774116209, 0.230878289967))
  expect_equal(island$ideal_island[[1]]$stac, 2)
  expect_equal(island$ideal_island[[1]]$missing_species, 0)
  expect_equal(island$ideal_island[[2]]$branching_times,
               c(1.0000000000000, 0.0116755987724))
  expect_equal(island$ideal_island[[2]]$stac, 4)
  expect_equal(island$ideal_island[[2]]$missing_species, 0)
  expect_equal(island$empirical_island[[1]]$branching_times,
               c(1.000000000000, 0.949774116209, 0.230878289967))
  expect_equal(island$empirical_island[[1]]$stac, 2)
  expect_equal(island$empirical_island[[1]]$missing_species, 0)
  expect_equal(island$empirical_island[[2]]$branching_times,
               c(1.0000000000000, 0.0116755987724))
  expect_equal(island$empirical_island[[2]]$stac, 4)
  expect_equal(island$empirical_island[[2]]$missing_species, 0)
})