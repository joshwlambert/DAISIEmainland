test_that("use", {
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(
    mainland_scenario = 20
  )
  island_tbl <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  )

  daisie_data <- create_daisie_data(
    total_time = 1,
    island_tbl = island_tbl,
    mainland_clade = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  )
  all_colonisations <- daisie_data$ideal_island[[2]]$all_colonisations
  check_all_colonisations(all_colonisations)
})
