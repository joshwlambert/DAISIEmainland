test_that("plot_ihs_param_estimates (no save) runs silent without error", {
  expect_silent(param_estimates <- plot_ihs_param_estimates(
    param_set = 1,
    data_folder_path = file.path("testdata"),
    output_file_path = NULL,
    parameter = "mainland_ex"))
})

test_that("plot_ihs_param_estimates (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(plot_ihs_param_estimates(
    param_set = 1,
    data_folder_path = file.path("testdata"),
    output_file_path = output_filename,
    parameter = "mainland_ex"))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})
