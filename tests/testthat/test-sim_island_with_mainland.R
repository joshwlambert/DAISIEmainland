test_that("sim_island_with_mainland produces correct empty island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )

  island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = FALSE)

  expect_equal(island$ideal_islands[[1]][[1]]$island_age, 1)
  expect_equal(island$ideal_islands[[1]][[1]]$not_present, 10)
  expect_equal(island$empirical_islands[[1]][[1]]$island_age, 1)
  expect_equal(island$empirical_islands[[1]][[1]]$not_present, 10)
})

test_that("sim_island_mainland produces correct non-empty island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = FALSE)

  expect_equal(island$ideal_island[[1]][[1]]$island_age, 1)
  expect_equal(island$ideal_island[[1]][[1]]$not_present, 8)
  expect_equal(island$ideal_island[[1]][[2]]$branching_times,
               c(1.000000000000, 0.380518542182))
  expect_equal(island$ideal_island[[1]][[2]]$stac, 2)
  expect_equal(island$ideal_island[[1]][[2]]$missing_species, 0)
  expect_equal(island$empirical_island[[1]][[1]]$island_age, 1)
  expect_equal(island$empirical_island[[1]][[1]]$not_present, 8)
  expect_equal(island$empirical_island[[1]][[2]]$branching_times,
               c(1.000000000000, 0.380518542182))
  expect_equal(island$empirical_island[[1]][[2]]$stac, 2)
  expect_equal(island$empirical_island[[1]][[2]]$missing_species, 0)
})

test_that("sim_island_with_mainland runs silent with verbose = FALSE", {
  expect_silent(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = FALSE)
  )
})

test_that("sim_island_with_mainland produces output with verbose = TRUE", {
  expect_output(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = TRUE),
    regexp = "Island replicate 1")
})


test_that("sim_island_mainland fails with incorrect input", {
  expect_error(island <- sim_island_with_mainland(
    time = "nonsense",
    m = 10,
    island_pars = c(1,1,10,1,1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = "nonsense",
    island_pars = c(1,1,10,1,1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = "nonsense",
    mainland_ex = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1,1,10,1,1),
    mainland_ex = "nonsense",
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1,1,10,1,1),
    mainland_ex = 1,
    mainland_sample_prob = "nonsense",
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1,1,10,1,1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    replicates = "nonsense",
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1,1,10,1,1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = "nonsense")
  )
})
