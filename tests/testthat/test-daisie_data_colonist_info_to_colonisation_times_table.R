test_that("Detects a header", { # nolint indeed, this is complex :-)
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
    daisie_data_colonist_info_to_colonisation_times_table(
      daisie_data_colonist_info = daisie_header)
  )
  expect_silent(
    daisie_data_colonist_info_to_colonisation_times_table(
      daisie_data_colonist_info = daisie_non_header)
  )
})

test_that("Multiple recolonisations", { # nolint indeed, this is complex :-)
  # For Issuse 68, Issue #68
  seed <- 1912
  set.seed(
    seed,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1,
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
  t <- daisie_data_colonist_info_to_colonisation_times_table(
    daisie_data_colonist_info = daisie_data_colonist_info)
  expect_true("colonist_index" %in% names(t))
  expect_true("colonist_species_type" %in% names(t))
  expect_true("colonisation_time" %in% names(t))
})
