test_that("calc_rates produces correct output", {
  rates <- calc_rates(
    timeval = 0.5,
    total_time = 1.0,
    gam = 1.0,
    laa = 1.0,
    lac = 1.0,
    mu = 1.0,
    k = 10.0,
    num_spec = 1,
    num_immigrants = 1,
    mainland_n = 10)
  expect_equal(rates$immig_rate, 9.0)
  expect_equal(rates$ext_rate, 1.0)
  expect_equal(rates$ana_rate, 1.0)
  expect_equal(rates$clado_rate, 0.9)
})

test_that("calc_rates produces correct error messages", {
  skip("TODO")
  # RJCB: well, if you really want use a function with 10 arguments,
  # (spoiler: one shouldn't)
  # one get these lengthy tests as the ones below. The technical term
  # is that the 10-argument approach 'does not scale'
  #
  # The superior way would be:
  #
  #  expect_silent(
  #    calc_rates(create_test_somethings()) # All parameters have default value
  #  )
  #
  #  expect_error(
  #    calc_rates(create_test_somethings(timeval = -123.456)), # Misdefine only the desired parameter # nolint indeed a long line, but there was where it fit best
  #    "timeval must be positive"
  #  )

  expect_error(
    calc_rates(
      timeval = -123.456, #
      total_time = 1.0,
      gam = 1.0,
      laa = 1.0,
      lac = 1.0,
      mu = 1.0,
      k = 10.0,
      num_spec = 1,
      num_immigrants = 1,
      mainland_n = 10
    ),
    "time must be positive"
  )
  expect_error(
    calc_rates(
      timeval = 0.5,
      total_time = -123.456, #
      gam = 1.0,
      laa = 1.0,
      lac = 1.0,
      mu = 1.0,
      k = 10.0,
      num_spec = 1,
      num_immigrants = 1,
      mainland_n = 10
    ),
    "total_time must be positive"
  )
  expect_error(
    calc_rates(
      timeval = 0.5,
      total_time = 1.0,
      gam = -123.456, #
      laa = 1.0,
      lac = 1.0,
      mu = 1.0,
      k = 10.0,
      num_spec = 1,
      num_immigrants = 1,
      mainland_n = 10
    ),
    "gam must be positive"
  )
  expect_error(
    calc_rates(
      timeval = 0.5,
      total_time = 1.0,
      gam = 1.0,
      laa = -123.456, #
      lac = 1.0,
      mu = 1.0,
      k = 10.0,
      num_spec = 1,
      num_immigrants = 1,
      mainland_n = 10
    ),
    "laa must be positive"
  )
  expect_error(
    calc_rates(
      timeval = 0.5,
      total_time = 1.0,
      gam = 1.0,
      laa = 1.0,
      lac = -123.456, #
      mu = 1.0,
      k = 10.0,
      num_spec = 1,
      num_immigrants = 1,
      mainland_n = 10
    ),
    "lac must be positive"
  )
  expect_error(
    calc_rates(
      timeval = 0.5,
      total_time = 1.0,
      gam = 1.0,
      laa = 1.0,
      lac = 1.0,
      mu = -123.456, #
      k = 10.0,
      num_spec = 1,
      num_immigrants = 1,
      mainland_n = 10
    ),
    "mu must be positive"
  )
  expect_error(
    calc_rates(
      timeval = 0.5,
      total_time = 1.0,
      gam = 1.0,
      laa = 1.0,
      lac = 1.0,
      mu = 1.0,
      k = -123.456, #
      num_spec = 1,
      num_immigrants = 1,
      mainland_n = 10
    ),
    "k must be positive"
  )
  expect_error(
    calc_rates(
      timeval = 0.5,
      total_time = 1.0,
      gam = 1.0,
      laa = 1.0,
      lac = 1.0,
      mu = 1.0,
      k = 3.15, #
      num_spec = 1,
      num_immigrants = 1,
      mainland_n = 10
    ),
    "k must be a whole number"
  )
  expect_error(
    calc_rates(
      timeval = 0.5,
      total_time = 1.0,
      gam = 1.0,
      laa = 1.0,
      lac = 1.0,
      mu = 1.0,
      k = 10.0,
      num_spec = -123456789, #
      num_immigrants = 1,
      mainland_n = 10
    ),
    "num_spec must be positive"
  )
  expect_error(
    calc_rates(
      timeval = 0.5,
      total_time = 1.0,
      gam = 1.0,
      laa = 1.0,
      lac = 1.0,
      mu = 1.0,
      k = 10.0,
      num_spec = 3.14, #
      num_immigrants = 1,
      mainland_n = 10
    ),
    "num_spec must be a whole number"
  )
  expect_error(
    calc_rates(
      timeval = 0.5,
      total_time = 1.0,
      gam = 1.0,
      laa = 1.0,
      lac = 1.0,
      mu = 1.0,
      k = 10.0,
      num_spec = 1,
      num_immigrants = -123456, #
      mainland_n = 10
    ),
    "num_immigrants must be positive"
  )
  expect_error(
    calc_rates(
      timeval = 0.5,
      total_time = 1.0,
      gam = 1.0,
      laa = 1.0,
      lac = 1.0,
      mu = 1.0,
      k = 10.0,
      num_spec = 1,
      num_immigrants = 1,
      mainland_n = -123456789
    ),
    "mainland_n must be positive"
  )
  expect_error(
    calc_rates(
      timeval = 0.5,
      total_time = 1.0,
      gam = 1.0,
      laa = 1.0,
      lac = 1.0,
      mu = 1.0,
      k = 10.0,
      num_spec = 1,
      num_immigrants = 1,
      mainland_n = 3.14 #
    ),
    "mainland_n must be a whole number"
  )
})
