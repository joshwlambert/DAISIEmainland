test_that("format_to_daisie_data works with empty island", {
  skip("this is an old function that will be deleted")
  ideal_island <- list(list(branching_times = 1,
                            stac = 0,
                            missing_species = 0))
  empirical_island <- list(list(branching_times = 1,
                           stac = 0,
                           missing_species = 0))
  island_clade_1 <- list(ideal_island = ideal_island,
                         empirical_island = empirical_island)
  island_clade_2 <- list(ideal_island = ideal_island,
                         empirical_island = empirical_island)
  island_replicate <- list(island_clade_1,
                           island_clade_2)
  island_replicates <- list(island_replicate)
  daisie_data <- format_to_daisie_data(island_replicates = island_replicates,
                                       total_time = 1,
                                       m = 2)
  if (1 == 2) {
    expect_silent(check_daisie_data(daisie_data))
  }

  expect_length(daisie_data, 2)
  expect_named(daisie_data, c("ideal_islands", "empirical_islands"))
  expect_equal(daisie_data$ideal_islands[[1]],
               list(list(island_age = 1,
                         not_present = 2)))
  expect_equal(daisie_data$empirical_islands[[1]],
               list(list(island_age = 1,
                         not_present = 2)))
})

test_that("format_to_daisie_data works with non-empty island", {
  skip("this is an old function that will be deleted")
  ideal_island <- list(list(branching_times = c(1, 0.057),
                            stac = 4,
                            missing_species = 0))
  empirical_island <- list(list(branching_times = c(1, 0.057),
                                stac = 4,
                                missing_species = 0))
  island_clade_1 <- list(ideal_island = ideal_island,
                         empirical_island = empirical_island)
  island_clade_2 <- list(ideal_island = ideal_island,
                         empirical_island = empirical_island)
  island_replicate <- list(island_clade_1,
                           island_clade_2)
  island_replicates <- list(island_replicate)
  daisie_data <- format_to_daisie_data(island_replicates = island_replicates,
                                       total_time = 1,
                                       m = 2)

  expect_silent(check_daisie_data(daisie_data))

  expect_equal(daisie_data$ideal_islands[[1]],
               list(list(island_age = 1,
                         not_present = 0),
                    list(branching_times = c(1, 0.057),
                         stac = 4,
                         missing_species = 0),
                    list(branching_times = c(1, 0.057),
                         stac = 4,
                         missing_species = 0)))
  expect_equal(daisie_data$empirical_islands[[1]],
               list(list(island_age = 1,
                         not_present = 0),
                    list(branching_times = c(1, 0.057),
                         stac = 4,
                         missing_species = 0),
                    list(branching_times = c(1, 0.057),
                         stac = 4,
                         missing_species = 0)))
})
