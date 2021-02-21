test_that("create_test_mainland produces correct output for scenario 1", {
  mainland <- create_test_mainland(mainland_scenario = 1)
  expect_true(is.list(mainland))
  expect_true(is.data.frame(mainland[[1]]))
  expect_true(ncol(mainland[[1]]) == 9)
  expect_true(nrow(mainland[[1]]) == 1)
})

test_that("create_test_mainland produces correct output for scenario 2", {
  mainland <- create_test_mainland(mainland_scenario = 2)
  expect_true(is.list(mainland))
  expect_true(is.data.frame(mainland[[1]]))
  expect_true(ncol(mainland[[1]]) == 9)
  expect_true(nrow(mainland[[1]]) == 3)
})

test_that("create_test_mainland produces correct output for scenario 3", {
  mainland <- create_test_mainland(mainland_scenario = 3)
  expect_true(is.list(mainland))
  expect_true(is.data.frame(mainland[[1]]))
  expect_true(ncol(mainland[[1]]) == 9)
  expect_true(nrow(mainland[[1]]) == 3)
})

test_that("create_test_mainland produces correct output for scenario 4", {
  mainland <- create_test_mainland(mainland_scenario = 4)
  expect_true(is.list(mainland))
  expect_true(is.data.frame(mainland[[1]]))
  expect_true(ncol(mainland[[1]]) == 9)
  expect_true(nrow(mainland[[1]]) == 3)
})
