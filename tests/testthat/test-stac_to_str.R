test_that("use", {
  expect_equal(stac_to_str(0), "[no colonization has taken place]")
  expect_equal(stac_to_str(1), "Non_endemic_MaxAge")
  expect_equal(stac_to_str(2), "Endemic")
  expect_equal(stac_to_str(3), "Endemic&Non_Endemic")
  expect_equal(stac_to_str(4), "Non_endemic")
  expect_equal(stac_to_str(5), "Endemic_singleton_MaxAge")
  expect_equal(stac_to_str(6), "Endemic_clade_MaxAge")
  expect_error(stac_to_str(-1))
  expect_error(stac_to_str(7))
  expect_error(stac_to_str(NA))
  expect_error(stac_to_str(NULL))
  expect_error(stac_to_str(Inf))
  expect_error(stac_to_str(c(1, 2)))
  expect_error(stac_to_str(c()))
  expect_error(stac_to_str(list()))
  expect_error(stac_to_str("nonsense"))
})

test_that("int -> str -> int", {
  for (i in seq(from = 0, to = 6)) {
    expect_equal(i, str_to_stac(stac_to_str(i)))
  }
})
