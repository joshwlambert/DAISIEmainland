test_that("create_test_mainland_clade produces correct output for scenario 1", {
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  expect_true(is.data.frame(mainland_clade))
  expect_true(ncol(mainland_clade) == 7)
  expect_true(nrow(mainland_clade) == 1)
})

test_that("create_test_mainland_clade produces correct output for scenario 2", {
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
  expect_true(is.data.frame(mainland_clade))
  expect_true(ncol(mainland_clade) == 7)
  expect_true(nrow(mainland_clade) == 3)
})

test_that("create_test_mainland_clade produces correct output for scenario 3", {
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 3)
  expect_true(is.data.frame(mainland_clade))
  expect_true(ncol(mainland_clade) == 7)
  expect_true(nrow(mainland_clade) == 3)
})

test_that("create_test_mainland_clade produces correct output for scenario 4", {
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 4)
  expect_true(is.data.frame(mainland_clade))
  expect_true(ncol(mainland_clade) == 7)
  expect_true(nrow(mainland_clade) == 3)
})

test_that("create_test_mainland_clade produces correct output for scenario 5", {
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 5)
  expect_true(is.data.frame(mainland_clade))
  expect_true(ncol(mainland_clade) == 7)
  expect_true(nrow(mainland_clade) == 3)
})

test_that("create_test_mainland_clade produces correct output for scenario 6", {
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 6)
  expect_true(is.data.frame(mainland_clade))
  expect_true(ncol(mainland_clade) == 7)
  expect_true(nrow(mainland_clade) == 7)
})

test_that("create_test_mainland_clade produces correct output for scenario 7", {
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 7)
  expect_true(is.data.frame(mainland_clade))
  expect_true(ncol(mainland_clade) == 7)
  expect_true(nrow(mainland_clade) == 7)
})
