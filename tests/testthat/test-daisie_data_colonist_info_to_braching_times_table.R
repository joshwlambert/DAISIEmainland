test_that("Detects a header", {
  # For Issuse 68, Issue #68
  seed <- 1
  set.seed(
    seed,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1,
    m = 2,
    island_pars = c(1, 0.1, 30.0, 1.0, 5.0),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  expect_true(length(ideal_daisie_data) >= 2)
  daisie_header <- ideal_daisie_data[[1]]
  daisie_non_header <- ideal_daisie_data[[2]]
  expect_error(
    daisie_data_colonist_info_to_braching_times_table(
      daisie_data_colonist_info = daisie_header
    )
  )
  expect_silent(
    daisie_data_colonist_info_to_braching_times_table(
      daisie_data_colonist_info = daisie_non_header
    )
  )
})

test_that("One colonist clade, only a colonisation time", {
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

  # We expect to have at least 1 colonist,
  # hence if ideal_daisie_data has length 2 we have 1 colonist,
  # as the first element is the header
  expect_equal(length(ideal_daisie_data), 2)
  # First branching time is the island age, second is colonisataion time
  island_age <- ideal_daisie_data[[2]]$branching_times[1]
  colonisation_time <- ideal_daisie_data[[2]]$branching_times[2]
  n_branches <- length(ideal_daisie_data[[2]]$branching_times) - 2

  # Only use braching times
  t <- daisie_data_colonist_info_to_braching_times_table(
    daisie_data_colonist_info = ideal_daisie_data[[2]]
  )
  expect_true("colonist_index" %in% names(t))
  expect_true("branching_times" %in% names(t))
  expect_equal(n_branches, nrow(t))
  expect_false(total_time %in% t$branching_times)
  expect_false(island_age %in% t$branching_times)
  expect_false(colonisation_time %in% t$branching_times)
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

  # First branching time is the island age, second is colonisataion time
  island_age <- ideal_daisie_data[[2]]$branching_times[1]
  colonisation_time <- ideal_daisie_data[[2]]$branching_times[2]
  branching_time <- ideal_daisie_data[[2]]$branching_times[3]
  n_branches <- length(ideal_daisie_data[[2]]$branching_times) - 2
  expect_equal(1, n_branches)

  # Only use braching times
  t <- daisie_data_colonist_info_to_braching_times_table(
    daisie_data_colonist_info = ideal_daisie_data[[2]]
  )
  expect_true("colonist_index" %in% names(t))
  expect_true("branching_times" %in% names(t))
  expect_equal(n_branches, nrow(t))
  expect_false(total_time %in% t$branching_times)
  expect_false(island_age %in% t$branching_times)
  expect_false(colonisation_time %in% t$branching_times)
  expect_true(branching_time %in% t$branching_times)
})


test_that("Multiple recolonisations", {
  # For Issuse 68, Issue #68
  seed <- 1912
  set.seed(
    seed,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  total_time <- 1
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = total_time,
    m = 10,
    island_pars = c(1, 0.1, 30.0, 1.0, 5.0),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  simplified_ideal_daisie_data <- list()
  simplified_ideal_daisie_data[[1]] <- ideal_daisie_data[[1]]
  simplified_ideal_daisie_data[[2]] <- ideal_daisie_data[[9]]
  daisie_data <- simplified_ideal_daisie_data
  daisie_data_colonist_info <- daisie_data[[2]]
  t <- daisie_data_colonist_info_to_braching_times_table(
    daisie_data_colonist_info = daisie_data_colonist_info)
  expect_true("colonist_index" %in% names(t))
  expect_true("branching_times" %in% names(t))
  # Do not include the colonisation
  expect_false(total_time %in% t$branching_times)
})
