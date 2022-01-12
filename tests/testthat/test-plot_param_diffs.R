test_that("plot_param_diffs (no save) runs silent without error", {
  expect_silent(param_diffs <- plot_param_diffs(
    param_set = 1,
    data_folder_path = file.path("testdata"),
    output_file_path = NULL,
    parameter = "mainland_ex",
    signif = 2,
    scientific = FALSE,
    transform = NULL))
})

test_that("plot_param_diffs scientific (no save) runs silent without error", {
  expect_silent(param_diffs <- plot_param_diffs(
    param_set = 1,
    data_folder_path = file.path("testdata"),
    output_file_path = NULL,
    parameter = "mainland_ex",
    signif = 2,
    scientific = TRUE,
    transform = NULL))
})

test_that("plot_param_diffs (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(plot_param_diffs(
    param_set = 1,
    data_folder_path = file.path("testdata"),
    output_file_path = output_filename,
    parameter = "mainland_ex",
    signif = 2,
    scientific = FALSE,
    transform = NULL))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})

test_that("plot_param_diffs ihs (no save) runs silent without error", {
  expect_silent(param_diffs <- plot_param_diffs(
    param_set = 1,
    data_folder_path = file.path("testdata"),
    output_file_path = NULL,
    parameter = "mainland_ex",
    signif = 2,
    scientific = FALSE,
    transform = "ihs"))
})

test_that("plot_param_estimates (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(plot_param_diffs(
    param_set = 1,
    data_folder_path = file.path("testdata"),
    output_file_path = output_filename,
    parameter = "mainland_ex",
    signif = 2,
    scientific = FALSE,
    transform = "ihs"))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})
