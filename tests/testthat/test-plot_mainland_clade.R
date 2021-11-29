test_that("mainland_scenario = 1", {
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  plot_mainland_clade(mainland_clade)
  expect_equal(1 + 1, 2) # To prevent testthat 'empty test' warning
})

test_that("mainland_scenario = 2", {
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
  plot_mainland_clade(mainland_clade)
  expect_equal(1 + 1, 2) # To prevent testthat 'empty test' warning
})
