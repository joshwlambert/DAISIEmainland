test_that("calc_num_col runs silent with correct output", {
  meta <- list(island_age = 1, not_present = 98)
  stac_2 <- list(
    branching_times = c(1.0, 0.98, 0.01),
    stac = 2,
    missing_spec = 0
  )
  stac_3 <- list(
    branching_times = c(1.0, 0.36, 0.09),
    stac = 3,
    missing_spec = 0,
    all_colonisations = list(
      list(
        event_times = c(1.0, 0.36, 0.09),
        species_type = "C"
      ),
      list(
        event_times = c(1.0, 0.06),
        species_type = "I"
      )
    )
  )

  ideal_multi_daisie_data <- list(list(meta, stac_2, stac_3))
  empirical_multi_daisie_data <- list(list(meta, stac_2, stac_3))
  daisie_mainland_data <- list(
    ideal_multi_daisie_data = ideal_multi_daisie_data,
    empirical_multi_daisie_data = empirical_multi_daisie_data
  )

  expect_silent(ideal_sim_num_col <- calc_num_col(
    multi_daisie_data = daisie_mainland_data$ideal_multi_daisie_data
  ))
  expect_silent(empirical_sim_num_col <- calc_num_col(
    multi_daisie_data = daisie_mainland_data$empirical_multi_daisie_data
  ))

  expect_equal(ideal_sim_num_col, 3)
  expect_equal(empirical_sim_num_col, 3)
})
