test_that("ana_event produces correct output for a single species island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  updated_state <- ana_event(
    island_tbl = create_test_island_tbl(island_scenario = 1),
    max_spec_id = 1
  )
  expect_true(is.list(updated_state))
  expected_island_tbl <- create_test_island_tbl(island_scenario = 10)
  expect_equal(updated_state$island_tbl, expected_island_tbl)
  expect_equal(updated_state$max_spec_id, 2)
})

test_that("ana_event produces correct output for a  multi-species island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  updated_state <- ana_event(
    island_tbl = create_test_island_tbl(island_scenario = 36),
    max_spec_id = 3
  )
  expect_true(is.list(updated_state))
  expected_island_tbl <- create_test_island_tbl(island_scenario = 44)
  expect_equal(updated_state$island_tbl, expected_island_tbl)
  expect_equal(updated_state$max_spec_id, 4)
})

test_that("ana_event fails with incorrect input", {
  island_tbl <- create_test_island_tbl(island_scenario = 1)
  expect_error(ana_event(
    island_tbl = "nonsense",
    max_spec_id = 1
  ))
  expect_error(ana_event(
    island_tbl = island_tbl,
    max_spec_id = "nonsense"
  ))
})
