test_that("calc_max_age_percent runs without error", {
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
    replicates = 1,
    verbose = TRUE)

  expect_silent(max_age_percent <- calc_max_age_percent(
    daisie_data = daisie_data))
  expect_equal(length(max_age_percent), 2)
  expect_equal(max_age_percent$ideal_max_age, 0)
  expect_equal(max_age_percent$empirical_max_age, 0)
})

test_that("calc_max_age_percent fails with incorrect daisie data", {

  daisie_data <- "nonsense"
  expect_error(calc_max_age_percent(daisie_data = daisie_data))

})
