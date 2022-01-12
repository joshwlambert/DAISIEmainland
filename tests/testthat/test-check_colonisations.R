test_that("use", {
  skip("temp skip in refactor")
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(
    mainland_scenario = 20
  )
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  )
  colonisations <- island$ideal_island[[2]]$all_colonisations[[1]]
  check_colonisations(colonisations)
})
