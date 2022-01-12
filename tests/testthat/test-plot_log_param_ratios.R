test_that("plot_log_param_ratios (no save) runs silent without error", {
  expect_silent(param_estimates <- plot_log_param_ratios(
    param_set = 3,
    data_folder_path = file.path("testdata"),
    output_file_path = NULL,
    parameter = "mainland_ex",
    num_breaks = 4,
    signif = 2,
    scientific = FALSE
  ))
})

test_that("plot_log_param_estimates (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(plot_log_param_ratios(
    param_set = 3,
    data_folder_path = file.path("testdata"),
    output_file_path = output_filename,
    parameter = "mainland_ex",
    num_breaks = 4,
    signif = 2,
    scientific = FALSE
  ))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})
