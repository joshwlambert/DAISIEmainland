test_that("immig_event produces correct output for empty island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  timeval <- 0.1
  island_spec <- create_test_island_spec(island_scenario = 0)
  mainland_spec <- 1
  island_spec <- immig_event(
    timeval = timeval,
    island_spec = island_spec,
    mainland_spec = mainland_spec)
  expect_true(is.data.frame(island_spec))
  expected_island_spec <- data.frame(spec_id = 1,
                                     main_anc_id = 1,
                                     col_t = 0.1,
                                     spec_type = "I",
                                     branch_code = as.character(NA),
                                     branch_t = NaN,
                                     ana_origin = as.character(NA))
  expect_equal(island_spec, expected_island_spec)
})

test_that("immig_event produces correct output for recolonisation", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  timeval <- 0.5
  island_spec <- create_test_island_spec(island_scenario = 1)
  mainland_spec <- 1
  island_spec <- immig_event(
    timeval = timeval,
    island_spec = island_spec,
    mainland_spec = mainland_spec)
  expect_true(is.data.frame(island_spec))
  expected_island_spec <- data.frame(spec_id = 1,
                                     main_anc_id = 1,
                                     col_t = 0.5,
                                     spec_type = "I",
                                     branch_code = as.character(NA),
                                     branch_t = NaN,
                                     ana_origin = as.character(NA))
  expect_equal(island_spec, expected_island_spec)
})

test_that("immig_event produces correct output for second immigrant", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  timeval <- 0.5
  island_spec <- create_test_island_spec(island_scenario = 1)
  mainland_spec <- 2
  island_spec <- immig_event(
    timeval = timeval,
    island_spec = island_spec,
    mainland_spec = mainland_spec)
  expect_true(is.data.frame(island_spec))
  expected_island_spec <- data.frame(spec_id = c(1, 2),
                                     main_anc_id = c(1, 2),
                                     col_t = c(0.2, 0.5),
                                     spec_type = c("I", "I"),
                                     branch_code = c(as.character(NA),
                                                     as.character(NA)),
                                     branch_t = c(NaN, NaN),
                                     ana_origin = c(as.character(NA),
                                                    as.character(NA)))
  expect_equal(island_spec, expected_island_spec)
})

test_that("immig_event fails with incorrect input", {

  island_spec <- create_test_island_spec(island_scenario = 1)

  expect_error(immig_event(
    timeval = "nonsense",
    island_spec = island_spec,
    mainland_spec = 1)
  )

  expect_error(immig_event(
    timeval = 0.5,
    island_spec = "nonsense",
    mainland_spec = 1)
  )

  expect_error(immig_event(
    timeval = 0.5,
    island_spec = island_spec,
    mainland_spec = "nonsense")
  )
})
