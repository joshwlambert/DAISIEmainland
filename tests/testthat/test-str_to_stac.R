test_that("use", {
  expect_equal(str_to_stac("Non_endemic_MaxAge"), 1)
  expect_equal(str_to_stac("Endemic"), 2)
  expect_equal(str_to_stac("Endemic&Non_Endemic"), 3)
  # stac 1 == stac 4
  # https://github.com/joshwlambert/DAISIEmainland/issues/25
  expect_equal(str_to_stac("Non_endemic_MaxAge"), 1)
  expect_equal(str_to_stac("Endemic_singleton_MaxAge"), 5)
  expect_equal(str_to_stac("Endemic_clade_MaxAge"), 6)
})

test_that("str -> int -> str", {
  stac_strs <- c(
    "Non_endemic_MaxAge",
    "Endemic",
    "Endemic&Non_Endemic",
    "Non_endemic_MaxAge",
    "Endemic_singleton_MaxAge",
    "Endemic_clade_MaxAge"
  )
  for (stac_str in stac_strs) {
    expect_equal(stac_str, stac_to_str(str_to_stac(stac_str)))
  }
})
