test_that("use", {
  skip("temp skip for refactor")
  set.seed(
    4,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_data <- sim_island_with_mainland(
    total_time = 1.0,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1)
  expect_silent(check_daisie_data(daisie_data))
  tables <- daisie_data_to_tables(daisie_data)

})
