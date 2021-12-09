test_that("any_recols is correct for recolonists", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  island <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(1, 0, 10, 1, 1),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE)
  ideal <- any_recols(island = island$ideal_islands[[1]])
  empirical <- any_recols(island = island$empirical_islands[[1]])
  expect_true(ideal)
  expect_true(empirical)
})

test_that("any_recols is correct for no recolonists", {
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
  ideal <- any_recols(island = island$ideal_islands[[1]])
  empirical <- any_recols(island = island$empirical_islands[[1]])
  expect_false(ideal)
  expect_false(empirical)
})

test_that("any_recols fails with incorrect input", {
  island <- "nonsense"
  expect_error(any_recols(island = island))
})
