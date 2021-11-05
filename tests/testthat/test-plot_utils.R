test_that("calc_quantiles runs silent without error", {
  quantiles <- calc_quantiles(plotting_data = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  expect_length(quantiles, 5)
  expect_equal(
    quantiles,
    c(ymin = 1.45, lower = 3.25, middle = 5.50, upper = 7.75, ymax = 9.55))
})

test_that("calc_outliers runs silent without error", {
  outliers <- calc_outliers(plotting_data = c(1, 2, 3, 4, 5, 6, 7, 8 , 9, 10))
  expect_length(outliers, 2)
  expect_equal(outliers, c(1, 10))
})

test_that("calc_outliers runs silent without error when all values are equal", {
  outliers <- calc_outliers(plotting_data = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
  expect_length(outliers, 1)
  expect_equal(outliers, 1)
})

test_that("create_plot_breaks runs silent without error", {
  breaks <- create_plot_breaks(lower_lim = 0,
                               upper_lim = 10000,
                               accuracy = 0.1,
                               round_func = floor)
  expect_length(breaks, 3)
  expect_true(is.numeric(breaks))
  expect_equal(breaks, c(0, 74.2, 11013.2))
})

test_that("create_plot_labels runs silent without error", {
  labels <- create_plot_labels(lower_lim = 0,
                               upper_lim = 10000,
                               accuracy = 0.1,
                               round_func = floor)
  expect_length(labels, 3)
  expect_true(is.character(labels))
  expect_equal(labels, c("0", "74.2", "1.1e+4"))
})
