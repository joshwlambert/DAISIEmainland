test_that("Issue 68: recolonisation", {
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
  DAISIEmainland::plot_daisie_mainland_data(
    daisie_mainland_data = daisie_mainland_data,
    replicate_index = 1
  )
  daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]

  t_bt <- daisie_data_colonist_info_to_braching_times_table(
    daisie_data_colonist_info = daisie_data[[3]]
  )
  time_1 <- daisie_data[[3]]$all_colonisations[[1]]$event_times[2]
  time_2 <- daisie_data[[3]]$all_colonisations[[2]]$event_times[2]
  # Both colonisation times must be present in the table
  expect_true(time_1 %in% t_bt$branching_times)
  skip("Issue #68")
  expect_true(time_2 %in% t_bt$branching_times)
})
