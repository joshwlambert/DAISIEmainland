test_that("use", {
  skip("WIP, #42")
  set.seed(
    9,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland <- sim_mainland(
    total_time = 10,
    m = 10,
    mainland_ex = 1.0
  )

  # Clade goes extinct, but after island age
  mainland_clade <- mainland[[1]]

  plot_mainland_clade(mainland_clade)
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")

  plot_island_tbl(island)
  daisie_data <- format_to_daisie_data(
    island_replicates = island,
    total_time = total_time,
    m = m
  )

  ideal_daisie_data <- daisie_data$ideal_islands
  daisie_datalist <- ideal_daisie_data[[1]]
  t <- daisie_datalist_to_tables(daisie_datalist)

  expect_true("island_age" %in% names(t))
  expect_true("not_present" %in% names(t))
  expect_true("colonisations" %in% names(t))
})
