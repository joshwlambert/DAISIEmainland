test_that("any_recols is correct for recolonists", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(1, 0, 10, 1, 1),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  ideal <- any_recols(
    daisie_data = daisie_mainland_data$ideal_multi_daisie_data[[1]]
  )
  empirical <- any_recols(
    daisie_data = daisie_mainland_data$empirical_multi_daisie_data[[1]]
  )
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
  daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(1, 0, 10, 0.1, 1),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  ideal <- any_recols(
    daisie_data = daisie_mainland_data$ideal_multi_daisie_data[[1]]
  )
  empirical <- any_recols(
    daisie_data = daisie_mainland_data$empirical_multi_daisie_data[[1]]
  )
  expect_false(ideal)
  expect_false(empirical)
})

test_that("any_recols fails with incorrect input", {
  daisie_data <- "nonsense"
  expect_error(any_recols(daisie_data = daisie_data))
})
