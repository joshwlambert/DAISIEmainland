test_that("sim_island_with_mainland produces correct empty island", {
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- sim_island_with_mainland(
      total_time = 1,
      m = 10,
      island_pars = c(1, 1, 10, 0.1, 1),
      mainland_ex = 1,
      mainland_sample_prob = 1,
      mainland_sample_type = "undiscovered",
      replicates = 1,
      verbose = FALSE)
  expect_equal(
    daisie_mainland_data$ideal_multi_daisie_data[[1]][[1]]$island_age, 1
  )
  expect_equal(
    daisie_mainland_data$ideal_multi_daisie_data[[1]][[1]]$not_present, 10
  )
  expect_equal(
    daisie_mainland_data$empirical_multi_daisie_data[[1]][[1]]$island_age, 1
  )
  expect_equal(
    daisie_mainland_data$empirical_multi_daisie_data[[1]][[1]]$not_present, 10
  )
})

test_that("sim_island_mainland produces correct non-empty island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE)
  expect_equal(
    daisie_mainland_data$ideal_multi_daisie_data[[1]][[1]]$island_age, 1
  )
  expect_equal(
    daisie_mainland_data$ideal_multi_daisie_data[[1]][[1]]$not_present, 6
  )
  expect_equal(
    daisie_mainland_data$ideal_multi_daisie_data[[1]][[2]]$branching_times,
    c(1.000000000000, 0.4393665143)
  )
  expect_equal(
    daisie_mainland_data$ideal_multi_daisie_data[[1]][[2]]$stac, 2
  )
  expect_equal(
    daisie_mainland_data$ideal_multi_daisie_data[[1]][[2]]$missing_species, 0
  )
  expect_equal(
    daisie_mainland_data$empirical_multi_daisie_data[[1]][[1]]$island_age, 1
  )
  expect_equal(
    daisie_mainland_data$empirical_multi_daisie_data[[1]][[1]]$not_present, 6
  )
  expect_equal(
    daisie_mainland_data$empirical_multi_daisie_data[[1]][[2]]$branching_times,
    c(1.000000000000, 0.4393665143)
  )
  expect_equal(
    daisie_mainland_data$empirical_multi_daisie_data[[1]][[2]]$stac, 2
  )
  expect_equal(
    daisie_mainland_data$empirical_multi_daisie_data[[1]][[2]]$missing_species,
    0
  )
})

test_that("sim_island_with_mainland with 0 mainland_ex produces correct
          output", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1)
  expect_identical(daisie_mainland_data$ideal_multi_daisie_data,
                   daisie_mainland_data$empirical_multi_daisie_data)
})

test_that("sim_island_with_mainland with 0 mainland_ex and incomplete sampling
          produces correct output", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 0,
    mainland_sample_prob = 0.1,
    mainland_sample_type = "undiscovered",
    replicates = 1)
  expect_false(identical(daisie_mainland_data$ideal_multi_daisie_data,
                         daisie_mainland_data$empirical_multi_daisie_data))
})

test_that("sim_island_with_mainland runs silent with verbose = FALSE", {
  expect_silent(sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = FALSE)
  )
})

test_that("sim_island_with_mainland produces output with verbose = TRUE", {
  expect_message(sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = TRUE),
    regexp = "Island replicate 1")
})

test_that("sim_island_mainland fails with incorrect input", {
  expect_error(sim_island_with_mainland(
    total_time = "nonsense",
    m = 10,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(sim_island_with_mainland(
    total_time = 1,
    m = "nonsense",
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = "nonsense",
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c("nonsense", 1, 10, 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, "nonsense", 10, 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = TRUE)
    )

  expect_error(sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, "nonsense", 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, "nonsense", 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 1, "nonsense"),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = "nonsense",
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = "nonsense",
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "nonsense",
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = "nonsense",
    verbose = TRUE)
  )

  expect_error(sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = "nonsense")
  )
})

test_that("sim_island_with_mainland runs with 1 mainland clade without mainland
          extinction", {
  expect_silent(
    sim_island_with_mainland(
      total_time = 1,
      m = 1,
      island_pars = c(1, 1, 10, 0.1, 1),
      mainland_ex = 0,
      mainland_sample_prob = 1,
      mainland_sample_type = "complete",
      replicates = 1))
})

test_that("sim_island_with_mainland fails with 1 mainland clade with mainland
          extinction", {
  expect_error(
    sim_island_with_mainland(
      total_time = 1,
      m = 1,
      island_pars = c(1, 1, 10, 0.1, 1),
      mainland_ex = 1,
      mainland_sample_prob = 1,
      mainland_sample_type = "complete",
      replicates = 1),
    regexp = "Simulating with mainland extinction requires more than one clade")
})

test_that("sim_island_with_mainland with 0.0 time", {
  expect_silent(
    sim_island_with_mainland(
      total_time = 0.0,
      m = 10,
      island_pars = c(1, 1, 10, 0.1, 1),
      mainland_ex = 1,
      mainland_sample_prob = 1,
      mainland_sample_type = "complete",
      replicates = 1)
  )
})
