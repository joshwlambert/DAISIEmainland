test_that("use", {
  expect_error(check_daisie_data(NULL))
  expect_error(check_daisie_data(NA))
  expect_error(check_daisie_data(Inf))
  expect_error(check_daisie_data(c()))
  expect_error(check_daisie_data(list()))
  expect_error(check_daisie_data("nonsense"))
  expect_error(check_daisie_data(42))
  expect_error(check_daisie_data(3.14))

  set.seed(
    4,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_data <- sim_island_with_mainland(
    total_time = 1.0,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1)
  expect_silent(check_daisie_data(daisie_data))
  expect_error(check_daisie_data(daisie_data$ideal_islands))
  expect_error(check_daisie_data(daisie_data$empirical_islands))
})
