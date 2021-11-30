test_that("use", {
  expect_equal(str_to_stac("[no colonization has taken place]"), 0)
  expect_equal(str_to_stac("Non_endemic_MaxAge"), 1)
  expect_equal(str_to_stac("Endemic"), 2)
  expect_equal(str_to_stac("Endemic&Non_Endemic"), 3)
  expect_equal(str_to_stac("Non_endemic"), 4)
  expect_equal(str_to_stac("Endemic_singleton_MaxAge"), 5)
  expect_equal(str_to_stac("Endemic_clade_MaxAge"), 6)
  expect_error(str_to_stac(NULL))
  expect_error(str_to_stac("nonsense"))
  expect_error(str_to_stac(NA))
  expect_error(str_to_stac(Inf))
  expect_error(str_to_stac(c()))
  expect_error(str_to_stac(list()))
  expect_error(str_to_stac(42))
  expect_error(str_to_stac(3.14))
  expect_error(str_to_stac(TRUE))
})

test_that("str -> int -> str", {
  stac_strs <- c(
    "[no colonization has taken place]",
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
