test_that("No extant colonists", {
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
    replicates = 10,
    verbose = FALSE
  )
  ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  empirical_daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[1]]
  daisie_data <- ideal_daisie_data
  expect_silent(daisie_data_to_tables(ideal_daisie_data))
  expect_silent(daisie_data_to_tables(empirical_daisie_data))
})

test_that("One colonist clade", {
  set.seed(
    1,
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
    verbose = FALSE
  )
  ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  empirical_daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[1]]
  daisie_data <- ideal_daisie_data
  expect_silent(daisie_data_to_tables(ideal_daisie_data))
  expect_silent(daisie_data_to_tables(empirical_daisie_data))
})

test_that("Issue #68: recolonisation", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )

  daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 50,
    island_pars = c(1.0, 0.5, 10, 0.1, 0.5),
    mainland_ex = 2,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  DAISIEmainland::plot_daisie_mainland_data(
    daisie_mainland_data = daisie_mainland_data,
    replicate_index = 1
  )
  daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  t <- daisie_data_to_tables(daisie_data)
  time_1 <- daisie_data[[3]]$all_colonisations[[1]]$event_times[2]
  time_2 <- daisie_data[[3]]$all_colonisations[[2]]$event_times[2]
  # Both colonisation times must be present in the table
  expect_true(time_1 %in% t$colonisation_times$colonisation_time)
  expect_true(time_2 %in% t$colonisation_times$colonisation_time)
})
