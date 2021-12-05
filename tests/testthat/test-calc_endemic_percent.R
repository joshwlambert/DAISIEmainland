test_that("calc_endemic_percent runs without error", {
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

  expect_silent(endemic_percent <- calc_endemic_percent(
    daisie_data = daisie_data))
  expect_length(endemic_percent, 6)
  expect_equal(endemic_percent$ideal_endemic_percent, 40)
  expect_equal(endemic_percent$empirical_endemic_percent, 100)
  expect_equal(endemic_percent$ideal_endemics, 2)
  expect_equal(endemic_percent$ideal_non_endemics, 3)
  expect_equal(endemic_percent$empirical_endemics, 5)
  expect_equal(endemic_percent$empirical_non_endemics, 0)
})

test_that("calc_endemic_percent fails with incorrect daisie data", {

  daisie_data <- "nonsense"
  expect_error(calc_endemic_percent(daisie_data = daisie_data))

})
