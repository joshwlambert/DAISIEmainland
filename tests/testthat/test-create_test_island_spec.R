test_that("create_test_island_spec produces correct output for scenario 1", {
  island_spec <- create_test_island_spec(island_scenario = 1)
  expect_true(is.data.frame(island_spec))
  expect_true(ncol(island_spec) == 7)
  expect_true(nrow(island_spec) == 0)
})

test_that("create_test_island_spec produces correct output for scenario 2", {
  island_spec <- create_test_island_spec(island_scenario = 2)
  expect_true(is.data.frame(island_spec))
  expect_true(ncol(island_spec) == 7)
  expect_true(nrow(island_spec) == 1)
})

test_that("create_test_island_spec produces correct output for scenario 3", {
  island_spec <- create_test_island_spec(island_scenario = 3)
  expect_true(is.data.frame(island_spec))
  expect_true(ncol(island_spec) == 7)
  expect_true(nrow(island_spec) == 4)
})

test_that("create_test_island_spec produces correct output for scenario 4", {
  island_spec <- create_test_island_spec(island_scenario = 4)
  expect_true(is.data.frame(island_spec))
  expect_true(ncol(island_spec) == 7)
  expect_true(nrow(island_spec) == 4)
})

test_that("create_test_island_spec produces correct output for scenario 5", {
  island_spec <- create_test_island_spec(island_scenario = 5)
  expect_true(is.data.frame(island_spec))
  expect_true(ncol(island_spec) == 7)
  expect_true(nrow(island_spec) == 3)
})

test_that("create_test_island_spec produces correct output for scenario 6", {
  island_spec <- create_test_island_spec(island_scenario = 6)
  expect_true(is.data.frame(island_spec))
  expect_true(ncol(island_spec) == 7)
  expect_true(nrow(island_spec) == 4)
})

test_that("create_test_island_spec produces correct output for scenario 7", {
  island_spec <- create_test_island_spec(island_scenario = 7)
  expect_true(is.data.frame(island_spec))
  expect_true(ncol(island_spec) == 7)
  expect_true(nrow(island_spec) == 3)
})
