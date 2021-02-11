# RJCB: nowadays, it is recommended to remove this 'context' function
# Hadley said so, so it must be true :-)
context("calc_rates")

test_that("calc_rates produces correct output", {
  # RJCB: I love such a daring test, in which the exact output is tested!
  #
  # I would, however, prefer it even more would I have been able to write:
  #
  # rates <- calc_rates(create_test_rates_1())
  #
  # (feel free to use function names that are more inspired :-) )
  # Already from the signature, one can conclude that the change I suggest
  # is to a list. I feel that such a list can be referred to
  # as 'sim_parameters' in the documentation.
  rates <- calc_rates(
    timeval = 0.5,
    # RJCB: minor thing, feel free to ignore: '1.0' indicates a floating point,
    # and '1' an integer in most languages (such as C++). Setting 'totaltime'
    # to an integer value of 1 suggests to me that the time is
    # an amount (which are always integers), such as 'number of generations'.
    # Same for other parameters :-)
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
