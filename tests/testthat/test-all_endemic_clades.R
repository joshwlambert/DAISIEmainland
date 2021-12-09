test_that("all_endemic_clades is correct for all endemic clades", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  island <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(5, 0, 10, 0.1, 0),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE)
  ideal <- all_endemic_clades(island = island$ideal_islands[[1]])
  empirical <- all_endemic_clades(island = island$empirical_islands[[1]])
  expect_true(ideal)
  expect_true(empirical)
})

test_that("all_endemic_clades is correct for all endemic not all clades", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  island <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(1, 0, 10, 0.1, 20),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE)
  ideal <- all_endemic_clades(island = island$ideal_islands[[1]])
  empirical <- all_endemic_clades(island = island$empirical_islands[[1]])
  expect_false(ideal)
  expect_false(empirical)
})

test_that("all_endemic_clades is correct for not all endemic not all clades", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  island <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(1, 0, 10, 0.1, 1),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE)
  ideal <- all_endemic_clades(island = island$ideal_islands[[1]])
  empirical <- all_endemic_clades(island = island$empirical_islands[[1]])
  expect_false(ideal)
  expect_false(empirical)
})

test_that("all_endemic_clade is correct for all endemic clades with
          recolonisation", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  island <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1000, 0, 10, 100, 0),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE)
  ideal <- all_endemic_clades(island = island$ideal_islands[[1]])
  empirical <- all_endemic_clades(island = island$empirical_islands[[1]])
  expect_true(ideal)
  expect_true(empirical)
})

test_that("all_endemic_clades is correct for not all endemic clades with
          recolonisations", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  island <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(1, 0, 10, 1, 0),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE)
  ideal <- all_endemic_singletons(island = island$ideal_islands[[1]])
  empirical <- all_endemic_singletons(island = island$empirical_islands[[1]])
  expect_false(ideal)
  expect_false(empirical)
})

test_that("all_endemic_clades fails with incorrect input", {
  island <- "nonsense"
  expect_error(all_endemic_clades(island = island))
})
