test_that("use", {
  expect_error(check_daisie_mainland_data(NULL))
  expect_error(check_daisie_mainland_data(NA))
  expect_error(check_daisie_mainland_data(Inf))
  expect_error(check_daisie_mainland_data(c()))
  expect_error(check_daisie_mainland_data(list()))
  expect_error(check_daisie_mainland_data("nonsense"))
  expect_error(check_daisie_mainland_data(42))
  expect_error(check_daisie_mainland_data(3.14))

  set.seed(
    4,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1.0,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1
  )
  expect_silent(check_daisie_mainland_data(daisie_mainland_data))
  expect_error(check_daisie_mainland_data(
    daisie_mainland_data$ideal_multi_daisie_data
  ))
  expect_error(check_daisie_mainland_data(
    daisie_mainland_data$empirical_multi_daisie_data
  ))
})
