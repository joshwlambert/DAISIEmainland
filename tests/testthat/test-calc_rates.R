context("calc_rates")

test_that("calc_rates produces correct output", {
  rates <- calc_rates(
    timeval = 0.5,
    totaltime = 1,
    gam = 1,
    laa = 1,
    lac = 1,
    mu = 1,
    k = 10,
    num_spec = 1,
    num_immigrants = 1,
    mainland_n = 10)
  expect_equal(rates$immig_rate, 9)
  expect_equal(rates$ext_rate, 1)
  expect_equal(rates$ana_rate, 1)
  expect_equal(rates$clado_rate, 0.9)
})
