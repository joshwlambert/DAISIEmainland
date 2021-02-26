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
  # RJCB: well, if you really want use a function with 7 arguments,
  # (spoiler: one shouldn't)
  # one get these lengthy tests as the ones below. The technical term
  # is that the 7-argument approach 'does not scale'
  #
  # The superior way would be:
  #
  #  expect_silent(
  #    sim_island_with_mainland(create_test_somethings()) # All parameters have default value
  #  )
  #
  #  expect_error(
  #    sim_island_with_mainland(create_test_somethings(time = "nonsense")), # Misdefine only the desired parameter # nolint indeed a long line, but there was where it fit best
  #    "time must be a positive non-zero number"
  #  )


  # RJCB: no need to do 'island <-' if there will not be
  # output
  expect_error(island <- sim_island_with_mainland(
    time = "nonsense",
    m = 10,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = "nonsense",
    island_pars = c(1, 1, 10, 1, 1),
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

  # RJCB: a user would enjoy a better error message here
 if (1 == 2) {
    expect_error(sim_island_with_mainland(
      time = 1,
      m = 10,
      island_pars = c("nonsense", 1, 10, 1, 1),
      mainland_ex = 1,
      mainland_sample_prob = 1,
      replicates = 1,
      verbose = TRUE),
      "error with whatever that first island param is"
    )
    expect_error(sim_island_with_mainland(
      time = 1,
      m = 10,
      island_pars = c(1, "nonsense", 10, 1, 1),
      mainland_ex = 1,
      mainland_sample_prob = 1,
      replicates = 1,
      verbose = TRUE),
      "error with whatever that second island param is"
    )
    expect_error(sim_island_with_mainland(
      time = 1,
      m = 10,
      island_pars = c(1, 1, "nonsense", 1, 1),
      mainland_ex = 1,
      mainland_sample_prob = 1,
      replicates = 1,
      verbose = TRUE),
      "error with whatever that third island param is"
    )
    expect_error(sim_island_with_mainland(
      time = 1,
      m = 10,
      island_pars = c(1, 1, 10, "nonsense", 1),
      mainland_ex = 1,
      mainland_sample_prob = 1,
      replicates = 1,
      verbose = TRUE),
      "error with whatever that fourth island param is"
    )
    expect_error(sim_island_with_mainland(
      time = 1,
      m = 10,
      island_pars = c(1, 1, 10, 1, "nonsense"),
      mainland_ex = 1,
      mainland_sample_prob = 1,
      replicates = 1,
      verbose = TRUE),
      "error with whatever that fifth island param is"
    )
  }

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = "nonsense",
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = "nonsense",
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    replicates = "nonsense",
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = "nonsense")
  )
})
