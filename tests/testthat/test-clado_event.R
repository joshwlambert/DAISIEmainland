test_that("clado_event produces correct output on non-cladogenesis species", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  updated_state <- clado_event(
    timeval = 0.5,
    island_spec = create_test_island_spec(island_scenario = 1),
    max_spec_id = 1)
  expect_true(is.list(updated_state))
  expected_island_spec <- create_test_island_spec(island_scenario = 19)
  expect_equal(updated_state$island_spec, expected_island_spec)
  expect_equal(updated_state$max_spec_id, 3)
})

test_that("clado_event produces correct output on cladogenesis species", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  updated_state <- clado_event(
    timeval = 0.9,
    island_spec = create_test_island_spec(island_scenario = 19),
    max_spec_id = 3)
  expect_true(is.list(updated_state))
  expected_island_spec <- create_test_island_spec(island_scenario = 45)
  expect_equal(updated_state$island_spec, expected_island_spec)
  expect_equal(updated_state$max_spec_id, 5)
})

test_that("clado_event fails with incorrect input", {
  island_spec <- create_test_island_spec(island_scenario = 1)
  expect_error(clado_event(
    timeval = "nonsense",
    island_spec = island_spec,
    max_spec_id = 1)
  )
  expect_error(clado_event(
    timeval = 0.5,
    island_spec = "nonsense",
    max_spec_id = 1)
  )
  expect_error(clado_event(
    timeval = 0.5,
    island_spec = island_spec,
    max_spec_id = "nonsense")
  )
})
