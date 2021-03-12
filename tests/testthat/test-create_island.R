test_that("empty island is same as ideal", {
  set.seed(1)
  total_time <- 1
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  island_spec <- create_test_island_spec(island_scenario = 1)
  mainland_sample_prob <- 1

  island <- create_island(
    total_time = total_time,
    island_spec = island_spec,
    mainland_clade = mainland_clade,
    mainland_sample_prob = mainland_sample_prob
  )

  testthat::expect_identical(island$ideal_island, island$empirical_island)
  testthat::expect_length(island$ideal_island[[1]]$branching_times, 1)
  testthat::expect_length(island$empirical_island[[1]]$branching_times, 1)
})

test_that("mainland ancestor is extant; empirical = ideal", {
  set.seed(1)
  total_time <- 3

  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  island_spec <- create_test_island_spec(island_scenario = 2)
  mainland_sample_prob <- 1
  island <- create_island(
    total_time = total_time,
    island_spec = island_spec,
    mainland_clade = mainland_clade,
    mainland_sample_prob = mainland_sample_prob
  )
  testthat::expect_identical(island$ideal_island, island$empirical_island)
  testthat::expect_gt(length(island$ideal_island[[1]]$branching_times), 1)
  testthat::expect_gt(length(island$empirical_island[[1]]$branching_times), 1)
})

test_that("mainland ancestor is extinct; multiple colonists same species;
          no clado", {
  set.seed(1)
  total_time <- 3

  mainland_clade <- create_test_mainland_clade(mainland_scenario = 4)
  island_spec <- create_test_island_spec(island_scenario = 3)
  mainland_sample_prob <- 1

  island <- create_island(
    total_time = total_time,
    island_spec = island_spec,
    mainland_clade = mainland_clade,
    mainland_sample_prob = mainland_sample_prob
  )
  testthat::expect_false(
    identical(
      island$ideal_island,
      island$empirical_island
    )
  )
  testthat::expect_gt(length(island$ideal_island[[1]]$branching_times), 1)
  testthat::expect_gt(length(island$empirical_island[[1]]$branching_times), 1)
  testthat::expect_identical(island$empirical_island[[1]]$stac, 6)
  testthat::expect_identical(island$empirical_island[[2]]$stac, 6)
  testthat::expect_identical(island$ideal_island[[1]]$stac, 3)
  testthat::expect_identical(island$ideal_island[[2]]$stac, 3)
  testthat::expect_equal(
    island$empirical_island[[1]]$branching_times[1],
    island$empirical_island[[1]]$branching_times[2],
    tolerance = 1e-5
  )
  testthat::expect_equal(
    island$empirical_island[[2]]$branching_times[1],
    island$empirical_island[[2]]$branching_times[2],
    tolerance = 1e-5
  )
  testthat::expect_gt(
    island$ideal_island[[1]]$branching_times[1],
    island$ideal_island[[1]]$branching_times[2]
  )

  testthat::expect_gt(
    island$ideal_island[[2]]$branching_times[1],
    island$ideal_island[[2]]$branching_times[2]
  )
})

test_that("mainland ancestor is extinct; multiple colonists same species;
          clado event", {
  set.seed(1)
  total_time <- 3

  mainland_clade <- create_test_mainland_clade(mainland_scenario = 4)
  island_spec <- create_test_island_spec(island_scenario = 4)
  mainland_sample_prob <- 1

  island <- create_island(
    total_time = total_time,
    island_spec = island_spec,
    mainland_clade = mainland_clade,
    mainland_sample_prob = mainland_sample_prob
  )
  testthat::expect_false(
    identical(
      island$ideal_island,
      island$empirical_island
    )
  )
  testthat::expect_gt(length(island$ideal_island[[1]]$branching_times), 1)
  testthat::expect_gt(length(island$empirical_island[[1]]$branching_times), 1)

  # ideal has info on recolonist, empirical puts it in same clade
  testthat::expect_false(
    identical(
      length(island$empirical_island[[1]]), length(island$ideal_island[[1]])
    )
  )
  testthat::expect_lt(
      length(island$empirical_island[[1]]), length(island$ideal_island[[1]])
  )

  # Expect stac 2 and col time to be the minimum branching time. FAILS
  testthat::expect_identical(island$empirical_island[[1]]$stac, 2)
  testthat::expect_identical(island$ideal_island[[1]]$stac, 3)
  # Col time should be min branching time, not island age; FAILS
  testthat::expect_false(isTRUE(all.equal(
    island$empirical_island[[1]]$branching_times[1],
    island$empirical_island[[1]]$branching_times[2],
    tolerance = 1e-5
  )))
    testthat::expect_gt(
    island$ideal_island[[1]]$branching_times[1],
    island$ideal_island[[1]]$branching_times[2]
  )
})

test_that("mainland ancestor is extinct; only one colonists same species;
          clade and singleton cases", {
  set.seed(1)
  total_time <- 3

  mainland_clade <- create_test_mainland_clade(mainland_scenario = 4)
  island_spec <- create_test_island_spec(island_scenario = 5)
  mainland_sample_prob <- 1

  # sim_island(
  #   time = total_time,
  #   m = m,
  #   island_pars = island_pars,
  #   mainland_clade = mainland_clade,
  #   mainland_sample_prob = mainland_sample_prob
  # )

  island <- create_island(
    total_time = total_time,
    island_spec = island_spec,
    mainland_clade = mainland_clade,
    mainland_sample_prob = mainland_sample_prob
  )
  testthat::expect_false(
    identical(
      island$ideal_island,
      island$empirical_island
    )
  )
  testthat::expect_gt(length(island$ideal_island[[1]]$branching_times), 1)
  testthat::expect_gt(length(island$empirical_island[[1]]$branching_times), 1)
  # Singleton
  testthat::expect_identical(island$empirical_island[[1]]$stac, 5)
  # Clade
  # Currently 6, should be 2? Current output is that of "no branching event
  # between immigration and island age with extant descendants, and clade) FAILS
  testthat::expect_identical(island$empirical_island[[2]]$stac, 2)
  testthat::expect_identical(island$ideal_island[[1]]$stac, 3) # should be 2?
  testthat::expect_identical(island$ideal_island[[2]]$stac, 3) # should be 2?
  testthat::expect_equal(
    island$empirical_island[[1]]$branching_times[1],
    island$empirical_island[[1]]$branching_times[2],
    tolerance = 1e-5
  )
  testthat::expect_equal(
    island$empirical_island[[2]]$branching_times[1],
    island$empirical_island[[2]]$branching_times[2],
    tolerance = 1e-5
  )
  testthat::expect_gt(
    island$ideal_island[[1]]$branching_times[1],
    island$ideal_island[[1]]$branching_times[2]
  )

  testthat::expect_gt(
    island$ideal_island[[2]]$branching_times[1],
    island$ideal_island[[2]]$branching_times[2]
  )
})
