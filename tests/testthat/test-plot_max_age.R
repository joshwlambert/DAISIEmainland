test_that("plot_max_age (no save) runs silent without error", {
  expect_silent(max_age <- plot_max_age(
    data_folder_path = file.path("testdata"),
    output_file_path = NULL,
    parameter = "mainland_ex"))
})

test_that("plot_max_age (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(plot_max_age(
    data_folder_path = file.path("testdata"),
    output_file_path = output_filename,
    parameter = "mainland_ex"))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})
