test_that("plot_ihs (no save) runs silent without error", {
  expect_silent(ihs <- plot_ihs(
    output_file_path = NULL))
})

test_that("plot_ihs (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(plot_ihs(
    output_file_path = output_filename))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})


