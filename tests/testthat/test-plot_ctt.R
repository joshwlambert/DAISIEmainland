test_that("plot_ctt (no save) runs silent without error", {

  expect_silent(ctt <- plot_ctt(output_filename = output_filename,
                                save = FALSE,
                                test = TRUE))
})

test_that("plot_ctt (save) runs silent without error", {

  output_filename <- tempfile(
    pattern = "file",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(plot_ctt(output_filename = output_filename,
                         save = TRUE,
                         test = TRUE))

  file.remove(output_filename)
})
