test_that("immig_event produces correct output for empty island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  island_spec <- immig_event(
    timeval = 0.16,
    island_spec = create_test_island_spec(island_scenario = 0),
    mainland_spec = 1)
  expect_true(is.data.frame(island_spec))
  expected_island_spec <- create_test_island_spec(island_scenario = 1)
  expect_equal(island_spec, expected_island_spec)
})

test_that("immig_event produces correct output for recolonisation", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  island_spec <- immig_event(
    timeval = 0.5,
    island_spec = create_test_island_spec(island_scenario = 1),
    mainland_spec = 1)
  expect_true(is.data.frame(island_spec))
  expected_island_spec <- create_test_island_spec(island_scenario = 46)
  expect_equal(island_spec, expected_island_spec)
})

test_that("immig_event produces correct output for second immigrant", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  island_spec <- immig_event(
    timeval = 0.5,
    island_spec = create_test_island_spec(island_scenario = 1),
    mainland_spec = 2)
  expect_true(is.data.frame(island_spec))
  expected_island_spec <- create_test_island_spec(island_scenario = 47)
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
