test_that("format_to_daisie_data_core works with empty island", {
  island_clade_1 <- list(list(branching_times = 1,
                              stac = 0,
                              missing_species = 0))
  island_clade_2 <- list(list(branching_times = 1,
                              stac = 0,
                              missing_species = 0))
  island_replicates <- list(list(island_clade_1,
                                 island_clade_2))

  daisie_data <- format_to_daisie_data_core(
    island_replicates = island_replicates,
    total_time = 1,
    m = 2)
  expect_length(daisie_data, 1)
  expect_equal(daisie_data[[1]],
               list(list(island_age = 1,
                         not_present = 2)))
})

test_that("format_to_daisie_data works with non-empty island", {
  island_clade_1 <- list(list(branching_times = c(1, 0.057),
                              stac = 4,
                              missing_species = 0))
  island_clade_2 <- list(list(branching_times = c(1, 0.057),
                              stac = 4,
                              missing_species = 0))
  island_replicates <- list(list(island_clade_1,
                                 island_clade_2))

  daisie_data <- format_to_daisie_data_core(
    island_replicates = island_replicates,
    total_time = 1,
    m = 2)
  expect_length(daisie_data, 1)
  expect_length(daisie_data[[1]], 3)
  expect_equal(daisie_data[[1]],
               list(list(island_age = 1,
                         not_present = 0),
                    list(branching_times = c(1, 0.057),
                         stac = 4,
                         missing_species = 0),
                    list(branching_times = c(1, 0.057),
                         stac = 4,
                         missing_species = 0)))
})
