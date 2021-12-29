test_that("calc_param_diffs runs without error", {
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
  expect_silent(param_diffs <- calc_param_diffs(ideal_ml = ideal_ml,
                                                empirical_ml = empirical_ml))

  expect_length(param_diffs, 5)
  expect_equal(param_diffs$clado_diffs, 0.06)
  expect_equal(param_diffs$ext_diffs, 0.32)
  expect_equal(param_diffs$k_diffs, 39)
  expect_equal(param_diffs$immig_diffs, 0.02)
  expect_equal(param_diffs$ana_diffs, 0.04)
})

test_that("calc_param_diffs fails with incorrect ideal_ml", {
  ideal_ml <- "nonsense"

  empirical_ml <- list(data.frame("lambda_c" = 0.49,
                                  "mu" = 0.50,
                                  "K" = 13,
                                  "gamma" = 0.06,
                                  "lambda_a" = 0.76,
                                  "loglik" = -86.01,
                                  "df" = 5,
                                  "conv" = 0))
  expect_error(calc_param_diffs(ideal_ml = ideal_ml,
                                empirical_ml = empirical_ml))

})

test_that("calc_param_diffs fails with incorrect empirical_ml", {
  ideal_ml <- list(data.frame("lambda_c" = 0.55,
                              "mu" = 0.82,
                              "K" = 52,
                              "gamma" = 0.08,
                              "lambda_a" = 0.80,
                              "loglik" = -81.56,
                              "df" = 5,
                              "conv" = 0))

  empirical_ml <- "nonsense"
  expect_error(calc_param_diffs(ideal_ml = ideal_ml,
                                empirical_ml = empirical_ml))

})
