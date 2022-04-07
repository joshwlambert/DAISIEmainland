test_that("No extant colonists", {
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  total_time <- 1
  n_species_mainland <- 10
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = total_time,
    m = n_species_mainland,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 10,
    verbose = FALSE
  )
  ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  empirical_daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[1]]

  # No colonists, no glory
  ideal_tables <- daisie_data_to_tables(ideal_daisie_data)
  expect_true("header" %in% names(ideal_tables))
  expect_equal(total_time, ideal_tables$header$island_age)
  expect_equal(n_species_mainland, ideal_tables$header$not_present)
  expect_equal(0, nrow(ideal_tables$colonists_general))
  expect_equal(0, nrow(ideal_tables$colonists_branching_times))

  # No colonists, no glory
  empirical_tables <- daisie_data_to_tables(empirical_daisie_data)
  expect_true("header" %in% names(empirical_tables))
  expect_equal(total_time, empirical_tables$header$island_age)
  expect_equal(n_species_mainland, empirical_tables$header$not_present)
  expect_equal(0, nrow(empirical_tables$colonists_general))
  expect_equal(0, nrow(empirical_tables$colonists_branching_times))
})


test_that("One colonist clade, only a colonisation, i.e. no branching", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  total_time <- 1
  n_species_mainland <- 10
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = total_time,
    m = n_species_mainland,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = FALSE
  )
  ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  empirical_daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[1]]

  ideal_tables <- daisie_data_to_tables(daisie_data = ideal_daisie_data)

  # One colonist
  expect_equal(nrow(ideal_tables$colonists_general), 1)
  expect_equal(nrow(ideal_tables$colonisation_times), 1)

  # Only colonisation time
  expect_equal(nrow(ideal_tables$colonists_branching_times), 0)

})

test_that("One colonist clade, with one branch", {
  set.seed(
    5,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  total_time <- 1
  n_species_mainland <- 10
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = total_time,
    m = n_species_mainland,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = FALSE
  )
  ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  expect_equal(length(ideal_daisie_data), 2)
  expect_equal(length(ideal_daisie_data[[2]]$branching_times), 3)

  ideal_tables <- daisie_data_to_tables(daisie_data = ideal_daisie_data)

  # One colonist
  expect_equal(nrow(ideal_tables$colonists_general), 1)
  expect_equal(nrow(ideal_tables$colonisation_times), 1)

  # One colonisation time
  expect_equal(nrow(ideal_tables$colonists_branching_times), 1)
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
  daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  t <- daisie_data_to_tables(daisie_data)
  time_1 <- daisie_data[[3]]$all_colonisations[[1]]$event_times[2]
  time_2 <- daisie_data[[3]]$all_colonisations[[2]]$event_times[2]
  # Both colonisation times must be present in the table
  expect_true(time_1 %in% t$colonisation_times$colonisation_time)
  expect_true(time_2 %in% t$colonisation_times$colonisation_time)
})
