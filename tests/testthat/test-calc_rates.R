test_that("calc_rates produces correct output", {
  rates <- calc_rates(
    gam = 1.0,
    laa = 1.0,
    lac = 1.0,
    mu = 1.0,
    k = 10.0,
    num_spec = 1,
    num_immigrants = 1,
    mainland_n = 10
  )
  expect_equal(rates$immig_rate, 9.0)
  expect_equal(rates$ext_rate, 1.0)
  expect_equal(rates$ana_rate, 1.0)
  expect_equal(rates$clado_rate, 0.9)
})

test_that("calc_rates fails with incorrect input", {
  expect_error(calc_rates(
    gam = "nonsense",
    laa = 1.0,
    lac = 1.0,
    mu = 1.0,
    k = 10.0,
    num_spec = 1,
    num_immigrants = 1,
    mainland_n = 10
  ))

  expect_error(calc_rates(
    gam = 1.0,
    laa = "nonsense",
    lac = 1.0,
    mu = 1.0,
    k = 10.0,
    num_spec = 1,
    num_immigrants = 1,
    mainland_n = 10
  ))

  expect_error(calc_rates(
    gam = 1.0,
    laa = 1.0,
    lac = "nonsense",
    mu = 1.0,
    k = 10.0,
    num_spec = 1,
    num_immigrants = 1,
    mainland_n = 10
  ))

  expect_error(calc_rates(
    gam = 1.0,
    laa = 1.0,
    lac = 1.0,
    mu = "nonsense",
    k = 10.0,
    num_spec = 1,
    num_immigrants = 1,
    mainland_n = 10
  ))

  expect_error(calc_rates(
    gam = 1.0,
    laa = 1.0,
    lac = 1.0,
    mu = 1.0,
    k = "nonsense",
    num_spec = 1,
    num_immigrants = 1,
    mainland_n = 10
  ))

  expect_error(calc_rates(
    gam = 1.0,
    laa = 1.0,
    lac = 1.0,
    mu = 1.0,
    k = 10.0,
    num_spec = "nonsense",
    num_immigrants = 1,
    mainland_n = 10
  ))

  expect_error(calc_rates(
    gam = 1.0,
    laa = 1.0,
    lac = 1.0,
    mu = 1.0,
    k = 10.0,
    num_spec = 1,
    num_immigrants = "nonsense",
    mainland_n = 10
  ))

  expect_error(calc_rates(
    gam = 1.0,
    laa = 1.0,
    lac = 1.0,
    mu = 1.0,
    k = 10.0,
    num_spec = 1,
    num_immigrants = 1,
    mainland_n = "nonsense"
  ))
})
