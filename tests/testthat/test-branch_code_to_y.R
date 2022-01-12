test_that("use", {
  expect_equal(branch_code_to_y(branch_code = "A"), 1.0 / 2.0)
  expect_equal(branch_code_to_y(branch_code = "AA"),
    1.0 / 4.0,
    tolerance = 0.01
  )
  expect_equal(branch_code_to_y(branch_code = "AB"),
    3.0 / 4.0,
    tolerance = 0.01
  )

  expect_equal(branch_code_to_y(branch_code = "AAA"),
    1.0 / 8.0,
    tolerance = 0.01
  )
  expect_equal(branch_code_to_y(branch_code = "AAB"),
    3.0 / 8.0,
    tolerance = 0.01
  )
  expect_equal(branch_code_to_y(branch_code = "ABA"),
    5.0 / 8.0,
    tolerance = 0.01
  )
  expect_equal(branch_code_to_y(branch_code = "ABB"),
    7.0 / 8.0,
    tolerance = 0.01
  )

  expect_true(branch_code_to_y(branch_code = "ABAB") < 1.0)
})
