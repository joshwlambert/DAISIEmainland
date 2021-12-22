test_that("multiplication works", {
  island <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(1, 1, 50, 0.1, 1),
    mainland_ex = 0.5,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  ideal_daisie_data <- island$ideal_islands
  daisie_datalist <- ideal_daisie_data[[1]]
  t <- daisie_datalist_to_tables(daisie_datalist)

  expect_true("island_age" %in% names(t))
  expect_true("not_present" %in% names(t))
  expect_true("colonisations" %in% names(t))
})
