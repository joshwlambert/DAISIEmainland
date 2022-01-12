test_that("group_multi_daisie_data works with empty island", {
  skip("WIP")
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  island_tbl_list <- list()
  daisie_data_list <- list()
  for (i in 1:5) {
    island_tbl_list[[i]] <- create_test_island_tbl(island_scenario = 1)
    daisie_data_list[[i]] <- create_daisie_data(
      total_time = 1,
      island_tbl = island_tbl_list[[i]],
      mainland_clade = mainland_clade,
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
    )
  }
  multi_daisie_data <- list(daisie_data_list)

  daisie_mainland_data <- group_multi_daisie_data(
    multi_daisie_data = multi_daisie_data,
    total_time = total_time,
    m = m
  )
  if (1 == 2) {
    expect_silent(check_daisie_data(daisie_data))
  }

  expect_length(daisie_data, 2)
  expect_named(daisie_data, c("ideal_islands", "empirical_islands"))
  expect_equal(
    daisie_data$ideal_islands[[1]],
    list(list(
      island_age = 1,
      not_present = 2
    ))
  )
  expect_equal(
    daisie_data$empirical_islands[[1]],
    list(list(
      island_age = 1,
      not_present = 2
    ))
  )
})

test_that("group_multi_daisie_data works with non-empty island", {
  skip("WIP")
  ideal_island <- list(list(
    branching_times = c(1, 0.057),
    stac = 4,
    missing_species = 0
  ))
  empirical_island <- list(list(
    branching_times = c(1, 0.057),
    stac = 4,
    missing_species = 0
  ))
  island_clade_1 <- list(
    ideal_island = ideal_island,
    empirical_island = empirical_island
  )
  island_clade_2 <- list(
    ideal_island = ideal_island,
    empirical_island = empirical_island
  )
  island_replicate <- list(
    island_clade_1,
    island_clade_2
  )
  island_replicates <- list(island_replicate)
  daisie_data <- format_to_daisie_data(
    island_replicates = island_replicates,
    total_time = 1,
    m = 2
  )

  expect_silent(check_daisie_data(daisie_data))

  expect_equal(
    daisie_data$ideal_islands[[1]],
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
  expect_equal(
    daisie_data$empirical_islands[[1]],
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
