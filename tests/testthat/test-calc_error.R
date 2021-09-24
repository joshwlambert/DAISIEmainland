test_that("calc_error runs without error", {

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

  ideal_ml <- list(data.frame("lambda_c" = 0.55,
                         "mu" = 0.82,
                         "K" = 52,
                         "gamma" = 0.08,
                         "lambda_a" = 0.80,
                         "loglik" = -81.56,
                         "df" = 5,
                         "conv" = 0))

  empirical_ml <- list(data.frame("lambda_c" = 0.49,
                             "mu" = 0.50,
                             "K" = 13,
                             "gamma" = 0.06,
                             "lambda_a" = 0.76,
                             "loglik" = -86.01,
                             "df" = 5,
                             "conv" = 0))

  expect_silent(error <- calc_error(daisie_data = daisie_data,
                                    ideal_ml = ideal_ml,
                                    empirical_ml = empirical_ml))

  expect_equal(error$delta_ctt, 0.08134983)
  expect_equal(error$max_age_percent$ideal_max_age, 0)
  expect_equal(error$max_age_percent$empirical_max_age, 0)
  expect_equal(error$endemic_percent$ideal_endemic_percent, 100)
  expect_equal(error$endemic_percent$empirical_endemic_percent, 100)
  expect_equal(error$endemic_percent$ideal_endemics, 4)
  expect_equal(error$endemic_percent$ideal_non_endemics, 0)
  expect_equal(error$endemic_percent$empirical_endemics, 4)
  expect_equal(error$endemic_percent$empirical_non_endemics, 0)
  expect_equal(error$param_diffs$clado_diff, 0.06)
  expect_equal(error$param_diffs$ext_diff, 0.32)
  expect_equal(error$param_diffs$k_diff, 39)
  expect_equal(error$param_diffs$immig_diff, 0.02)
  expect_equal(error$param_diffs$ana_diff, 0.04)
})

test_that("calc_error fails with incorrect daisie data", {

  daisie_data <- "nonsense"

  ideal_ml <- list(data.frame("lambda_c" = 0.55,
                              "mu" = 0.82,
                              "K" = 52,
                              "gamma" = 0.08,
                              "lambda_a" = 0.80,
                              "loglik" = -81.56,
                              "df" = 5,
                              "conv" = 0))

  empirical_ml <- list(data.frame("lambda_c" = 0.49,
                                  "mu" = 0.50,
                                  "K" = 13,
                                  "gamma" = 0.06,
                                  "lambda_a" = 0.76,
                                  "loglik" = -86.01,
                                  "df" = 5,
                                  "conv" = 0))

  expect_error(calc_error(daisie_data = daisie_data,
                          ideal_ml = ideal_ml,
                          empirical_ml = empirical_ml))
})

test_that("calc_error fails with incorrect ideal_ml", {

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

  ideal_ml <- "nonsense"

  empirical_ml <- list(data.frame("lambda_c" = 0.49,
                                  "mu" = 0.50,
                                  "K" = 13,
                                  "gamma" = 0.06,
                                  "lambda_a" = 0.76,
                                  "loglik" = -86.01,
                                  "df" = 5,
                                  "conv" = 0))

  expect_error(calc_error(daisie_data = daisie_data,
                          ideal_ml = ideal_ml,
                          empirical_ml = empirical_ml))
})

test_that("calc_error fails with incorrect empirical_ml", {

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

  ideal_ml <- list(data.frame("lambda_c" = 0.55,
                              "mu" = 0.82,
                              "K" = 52,
                              "gamma" = 0.08,
                              "lambda_a" = 0.80,
                              "loglik" = -81.56,
                              "df" = 5,
                              "conv" = 0))

  empirical_ml <- "nonsense"

  expect_error(calc_error(daisie_data = daisie_data,
                          ideal_ml = ideal_ml,
                          empirical_ml = empirical_ml))
})
