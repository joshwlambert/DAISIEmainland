test_that("calc_island_endemics produces is correct for all endemics", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(0, 0, 10, 0.1, 100),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE)
  ideal <- calc_island_endemics(
    daisie_data = daisie_mainland_data$ideal_multi_daisie_data[[1]])
  empirical <- calc_island_endemics(
    daisie_data = daisie_mainland_data$empirical_multi_daisie_data[[1]])
  expect_equal(ideal, list(endemics = 6, non_endemics = 0))
  expect_equal(empirical, list(endemics = 6, non_endemics = 0))
})

test_that("calc_island_endemics produces is correct for all non-endemics", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(0, 0, 10, 0.1, 0),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE)
  ideal <- calc_island_endemics(
    daisie_data = daisie_mainland_data$ideal_multi_daisie_data[[1]])
  empirical <- calc_island_endemics(
    daisie_data = daisie_mainland_data$empirical_multi_daisie_data[[1]])
  expect_equal(ideal, list(endemics = 0, non_endemics = 5))
  expect_equal(empirical, list(endemics = 0, non_endemics = 5))
})

test_that("calc_island_endemics produces is correct for endemics and
          non-endemics", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE)
  ideal <- calc_island_endemics(
    daisie_data = daisie_mainland_data$ideal_multi_daisie_data[[1]])
  empirical <- calc_island_endemics(
    daisie_data = daisie_mainland_data$empirical_multi_daisie_data[[1]])
  expect_equal(ideal, list(endemics = 4, non_endemics = 2))
  expect_equal(empirical, list(endemics = 4, non_endemics = 2))
})

test_that("calc_island_endemics produces is correct for empty island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(1, 1, 10, 0.001, 1),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE)
  ideal <- calc_island_endemics(
    daisie_data = daisie_mainland_data$ideal_multi_daisie_data[[1]])
  empirical <- calc_island_endemics(
    daisie_data = daisie_mainland_data$empirical_multi_daisie_data[[1]])
  expect_equal(ideal, list(endemics = 0, non_endemics = 0))
  expect_equal(empirical, list(endemics = 0, non_endemics = 0))
})

test_that("calc_island_endemics produces is correct for all endemics with
          recolonisations", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(0, 0, 10, 1, 100),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE)
  ideal <- calc_island_endemics(
    daisie_data = daisie_mainland_data$ideal_multi_daisie_data[[1]])
  empirical <- calc_island_endemics(
    daisie_data = daisie_mainland_data$empirical_multi_daisie_data[[1]])
  expect_equal(ideal, list(endemics = 81, non_endemics = 0))
  expect_equal(empirical, list(endemics = 81, non_endemics = 0))
})

test_that("calc_island_endemics produces is correct for endemics and
          non-endemics with recolonisations", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_ex = 0,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE)
  ideal <- calc_island_endemics(
    daisie_data = daisie_mainland_data$ideal_multi_daisie_data[[1]])
  empirical <- calc_island_endemics(
    daisie_data = daisie_mainland_data$empirical_multi_daisie_data[[1]])
  expect_equal(ideal, list(endemics = 38, non_endemics = 30))
  expect_equal(empirical, list(endemics = 38, non_endemics = 30))
})
