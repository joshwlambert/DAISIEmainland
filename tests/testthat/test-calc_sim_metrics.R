test_that("multiplication works", {

  meta <- list(island_age = 1, not_present = 98)
  stac_2 <- list(branching_times = c(1.0, 0.98, 0.01),
                 stac = 2,
                 missing_spec = 0)
  stac_3 <- list(branching_times = c(1.0, 0.36, 0.09),
                 stac = 3,
                 missing_spec = 0,
                 all_colonisations = list(list(event_times = c(1.0, 0.36, 0.09),
                                               species_type = "C"),
                                          list(event_times = c(1.0, 0.06),
                                               species_type = "I")))

  ideal_islands <- list(list(meta, stac_2, stac_3))
  empirical_islands <- list(list(meta, stac_2, stac_3))
  island <- list(ideal_islands = ideal_islands,
                     empirical_islands = empirical_islands)

  expect_silent(ideal_sim_metrics <- DAISIEmainland::calc_sim_metrics(
    daisie_data = island$ideal_islands))
  expect_silent(empirical_sim_metrics <- DAISIEmainland::calc_sim_metrics(
    daisie_data = island$empirical_islands))

  expect_length(ideal_sim_metrics, 2)
  expect_length(empirical_sim_metrics, 2)
  expect_equal(ideal_sim_metrics$num_col, 3)
  expect_equal(ideal_sim_metrics$num_spec, 5)
  expect_equal(empirical_sim_metrics$num_col, 3)
  expect_equal(empirical_sim_metrics$num_spec, 5)
})
