test_that("use", {
  expect_error(check_daisie_datalist(NULL))
  expect_error(check_daisie_datalist(NA))
  expect_error(check_daisie_datalist(Inf))
  expect_error(check_daisie_datalist(c()))
  expect_error(check_daisie_datalist(list()))
  expect_error(check_daisie_datalist("nonsense"))
  expect_error(check_daisie_datalist(42))
  expect_error(check_daisie_datalist(3.14))

  island <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(1, 1, 50, 0.1, 1),
    mainland_ex = 0.5,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  ideal_daisie_data <- island$ideal_islands
  daisie_datalist <- ideal_daisie_data[[1]]
  expect_silent(check_daisie_datalist(daisie_datalist))

  expect_error(check_daisie_datalist(daisie_datalist[[1]]))

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
