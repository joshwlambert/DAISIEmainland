test_that("plot_max_age (no save) runs silent without error", {
  skip("Data files need replacing with new run")
  expect_silent(max_age <- plot_max_age(
    data_folder_path = file.path("testdata"),
    output_file_path = NULL))
})

test_that("plot_max_age (save) runs silent without error", {
  skip("Data files need replacing with new run")
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(plot_max_age(data_folder_path = file.path("testdata"),
                             output_file_path = output_filename))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})
