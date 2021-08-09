test_that("sample_event produces correct output for empty island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  event <- sample_event(rates = list(immig_rate = 1,
                                     ext_rate = 0,
                                     ana_rate = 0,
                                     clado_rate = 0))
  expect_equal(event, 1)
})

test_that("sample_event produces correct output for non-empty island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  event <- sample_event(rates = list(immig_rate = 0.9,
                                     ext_rate = 1,
                                     ana_rate = 1,
                                     clado_rate = 0.9))
  expect_equal(event, 3)
})

test_that("sammple_event fails with incorrect input", {

  expect_error(sample_event(rates = "nonsense"))

  expect_error(sample_event(rates = list(immig_rate = "nonsense",
                                         ext_rate = 0,
                                         ana_rate = 0,
                                         clado_rate = 0))
  )

  expect_error(sample_event(rates = list(immig_rate = 1,
                            ext_rate = "nonsense",
                            ana_rate = 0,
                            clado_rate = 0))
  )

  expect_error(sample_event(rates = list(immig_rate = 1,
                            ext_rate = 0,
                            ana_rate = "nonsense",
                            clado_rate = 0))
  )

  expect_error(sample_event(rates = list(immig_rate = 1,
                            ext_rate = 0,
                            ana_rate = 0,
                            clado_rate = "nonsense"))
  )
})
