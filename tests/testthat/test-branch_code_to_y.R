test_that("use", {
  skip("WIP")
  expect_equal(branch_code_to_y("A"), 0.5)
  expect_equal(branch_code_to_y("AA"), 0.25)
  expect_equal(branch_code_to_y("AB"), 0.75)
})
