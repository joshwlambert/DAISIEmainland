test_that("calc_quantiles runs silent without error", {
  quantiles <- calc_quantiles(plotting_data = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  expect_length(quantiles, 5)
  expect_equal(
    quantiles,
    c(ymin = 1.45, lower = 3.25, middle = 5.50, upper = 7.75, ymax = 9.55))
})

test_that("calc_outliers runs silent without error", {
  outliers <- calc_outliers(plotting_data = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  expect_length(outliers, 2)
  expect_equal(outliers, c(1, 10))
})

test_that("calc_outliers runs silent without error when all values are equal", {
  outliers <- calc_outliers(plotting_data = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
  expect_length(outliers, 1)
  expect_equal(outliers, 1)
})

test_that("create_labels runs silent without error", {
  labels <- create_labels(signif = 2)
  expect_true(is.function(labels))
})

test_that("scientific (TRUE) runs silent without error", {
  labels <- scientific(c(1, 2, 3, 4, 5), signif = 2, scientific = TRUE)
  expect_true(is.expression(labels))
  expect_length(labels, 5)
})

test_that("scientific (FALSE) runs silent without error", {
  labels <- scientific(c(1, 2, 3, 4, 5), signif = 2, scientific = FALSE)
  expect_true(is.expression(labels))
  expect_length(labels, 5)
})

test_that("choose_scientific runs silent without error", {
  labels <- choose_scientific(c(1e-5, 1, 1e5), signif = 2)
  expect_length(labels, 3)
  expect_true(is.character(labels))
  expect_equal(labels, c("1e-05", "1.00", "1e+05"))
})
