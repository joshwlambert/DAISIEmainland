test_that("add_metadata_to_daisie_data works with empty island", {
  skip("WIP")
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  island_tbl <- create_test_island_tbl(island_scenario = 1)
  daisie_data <- create_daisie_data(
    total_time = 1,
    island_tbl = island_tbl,
    mainland_clade = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  )
  daisie_data_list <- list(daisie_data)
  multi_daisie_data <- list(daisie_data_list)
  daisie_mainland_data <- group_multi_daisie_data(
    multi_daisie_data = multi_daisie_data,
    total_time = total_time,
    m = m
  )

  ideal_multi_daisie_data <- add_metadata_to_daisie_data(
    multi_daisie_data = daisie_mainland_data$ideal_multi_daisie_data,
    total_time = 1,
    m = 2
  )

  empirical_multi_daisie_data <- add_metadata_to_daisie_data(
    multi_daisie_data = daisie_mainland_data$empirical_multi_daisie_data,
    total_time = 1,
    m = 2
  )

  expect_length(ideal_multi_daisie_data, 1)
  expect_equal(
    daisie_data[[1]],
    list(list(
      island_age = 1,
      not_present = 2
    ))
  )
})

test_that("add_metadata_to_daisie_data works with non-empty island", {
  skip("WIP")
  island_clade_1 <- list(list(
    branching_times = c(1, 0.057),
    stac = 4,
    missing_species = 0
  ))
  island_clade_2 <- list(list(
    branching_times = c(1, 0.057),
    stac = 4,
    missing_species = 0
  ))
  island_replicates <- list(list(
    island_clade_1,
    island_clade_2
  ))

  daisie_data <- format_to_daisie_data_core(
    island_replicates = island_replicates,
    total_time = 1,
    m = 2
  )
  expect_length(daisie_data, 1)
  expect_length(daisie_data[[1]], 3)
  expect_equal(
    daisie_data[[1]],
    list(
      list(
        island_age = 1,
        not_present = 0
      ),
      list(
        branching_times = c(1, 0.057),
        stac = 4,
        missing_species = 0
      ),
      list(
        branching_times = c(1, 0.057),
        stac = 4,
        missing_species = 0
      )
    )
  )
})
