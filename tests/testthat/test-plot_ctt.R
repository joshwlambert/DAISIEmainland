test_that("plot_ctt (no save) runs silent without error", {
    expect_silent(ctt <- plot_ctt(
      data_folder_path = file.path("testdata"),
      output_file_path = NULL))
})

test_that("plot_ctt (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(plot_ctt(data_folder_path = file.path("testdata"),
                         output_file_path = output_filename))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})
