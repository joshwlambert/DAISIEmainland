test_that("calc_ctt runs without error", {

  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")

  daisie_data <- DAISIEmainland::sim_island_with_mainland(
    total_time = 5,
    m = 100,
    island_pars = c(0.1, 0.1, 10, 0.01, 0.1),
    mainland_ex = 0.5,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = TRUE)

  expect_silent(ctt <- calc_ctt(daisie_data = daisie_data))

  expect_equal(ctt, 0.08134983)
})

test_that("calc_ctt fails with incorrect daisie data", {

  daisie_data <- "nonsense"
  expect_error(calc_ctt(daisie_data = daisie_data))

})
