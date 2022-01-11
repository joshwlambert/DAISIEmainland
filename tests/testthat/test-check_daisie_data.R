test_that("use", {
  expect_error(check_daisie_data(NULL))
  expect_error(check_daisie_data(NA))
  expect_error(check_daisie_data(Inf))
  expect_error(check_daisie_data(c()))
  expect_error(check_daisie_data(list()))
  expect_error(check_daisie_data("nonsense"))
  expect_error(check_daisie_data(42))
  expect_error(check_daisie_data(3.14))

  daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(1, 1, 50, 0.1, 1),
    mainland_ex = 0.5,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  ideal_multi_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data
  daisie_data <- ideal_multi_daisie_data[[1]]
  expect_silent(check_daisie_data(daisie_data))

  expect_error(check_daisie_data(daisie_data[[1]]))

  # Only run locally, as DAISIE's output cannot be suppressed
  if (1 == 2) {
    # This is where daisie_data is used
    DAISIE::DAISIE_ML_CS(
      datalist = daisie_datalist,
      initparsopt = c(1, 1, 50, 0.1, 1),
      idparsopt = 1:5,
      parsfix = NULL,
      idparsfix = NULL,
      ddmodel = 11,
      methode = "odeint::runge_kutta_fehlberg78",
      optimmethod = "simplex",
      jitter = 1e-5
    )
  }
})
