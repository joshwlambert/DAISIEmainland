test_that("calc_param_ratios runs without error", {
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
  expect_silent(param_ratios <- calc_param_ratios(ideal_ml = ideal_ml,
                                                  empirical_ml = empirical_ml))

  expect_length(param_ratios, 5)
  expect_equal(param_ratios$clado_ratio, 1.12244897959)
  expect_equal(param_ratios$ext_ratio, 1.64)
  expect_equal(param_ratios$k_ratio, 4)
  expect_equal(param_ratios$immig_ratio, 1.33333333333)
  expect_equal(param_ratios$ana_ratio, 1.05263157895)
})

test_that("calc_param_ratios fails with incorrect ideal_ml", {
  ideal_ml <- "nonsense"

  empirical_ml <- list(data.frame("lambda_c" = 0.49,
                                  "mu" = 0.50,
                                  "K" = 13,
                                  "gamma" = 0.06,
                                  "lambda_a" = 0.76,
                                  "loglik" = -86.01,
                                  "df" = 5,
                                  "conv" = 0))
  expect_error(calc_param_ratios(ideal_ml = ideal_ml,
                                 empirical_ml = empirical_ml))

})

test_that("calc_param_ratios fails with incorrect empirical_ml", {
  ideal_ml <- list(data.frame("lambda_c" = 0.55,
                              "mu" = 0.82,
                              "K" = 52,
                              "gamma" = 0.08,
                              "lambda_a" = 0.80,
                              "loglik" = -81.56,
                              "df" = 5,
                              "conv" = 0))

  empirical_ml <- "nonsense"
  expect_error(calc_param_ratios(ideal_ml = ideal_ml,
                                 empirical_ml = empirical_ml))

})
