test_that("plot_param_estimates (no save) runs silent without error", {
  #warning from x-axis not plotting all points
  expect_warning(param_estimates <- plot_param_estimates(
    param_set = 1,
    xlim = TRUE,
    data_folder_path = file.path("testdata"),
    output_file_path = NULL,
    param_space = "general"))
})

test_that("plot_param_estimates (no save) runs silent without error", {
  expect_silent(param_estimates <- plot_param_estimates(
    param_set = 1,
    xlim = FALSE,
    data_folder_path = file.path("testdata"),
    output_file_path = NULL,
    param_space = "general"))
})

test_that("plot_param_estimates (save) runs silent without error", {
  #warning from x-axis not plotting all points
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_warning(plot_param_estimates(
    param_set = 1,
    xlim = TRUE,
    data_folder_path = file.path("testdata"),
    output_file_path = output_filename,
    param_space = "general"))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})

test_that("plot_param_estimates (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(plot_param_estimates(
    param_set = 1,
    xlim = FALSE,
    data_folder_path = file.path("testdata"),
    output_file_path = output_filename,
    param_space = "general"))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})
