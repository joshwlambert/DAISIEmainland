test_that("plot_endemics (no save) runs silent without error", {
  expect_silent(endemics <- plot_endemics(
    data_folder_path = file.path("testdata"),
    output_file_path = NULL,
    parameter = "both"))
})

test_that("plot_endemics (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(plot_endemics(
    data_folder_path = file.path("testdata"),
    output_file_path = output_filename,
    parameter = "both"))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})
