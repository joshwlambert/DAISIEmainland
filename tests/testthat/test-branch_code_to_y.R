test_that("use", {
  expect_equal(branch_code_to_y(branch_code = "A"), 0.5)
  expect_equal(branch_code_to_y(branch_code = "AA"), 0.33, tolerance = 0.01)
  expect_equal(branch_code_to_y("AB"), 0.66, tolerance = 0.01)
})
