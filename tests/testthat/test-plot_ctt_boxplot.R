test_that("plot_ctt_boxplot mainland_ex (no save) runs silent without error", {
  expect_silent(ctt <- plot_ctt_boxplot(
    data_folder_path = file.path("testdata"),
    output_file_path = NULL,
    parameter = "mainland_ex"))
})

test_that("plot_ctt_boxplot unsampled (no save) runs silent without error", {
  expect_silent(ctt <- plot_ctt_boxplot(
    data_folder_path = file.path("testdata"),
    output_file_path = NULL,
    parameter = "unsampled"))
})

test_that("plot_ctt_boxplot undiscovered (no save) runs silent without error", {
  expect_silent(ctt <- plot_ctt_boxplot(
    data_folder_path = file.path("testdata"),
    output_file_path = NULL,
    parameter = "undiscovered"))
})

test_that("plot_ctt_boxplot mainland_ex (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(plot_ctt_boxplot(data_folder_path = file.path("testdata"),
                                 output_file_path = output_filename,
                                 parameter = "mainland_ex"))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})

test_that("plot_ctt_boxplot unsampled (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(plot_ctt_boxplot(data_folder_path = file.path("testdata"),
                                 output_file_path = output_filename,
                                 parameter = "unsampled"))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})

test_that("plot_ctt_boxplot undiscovered (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(plot_ctt_boxplot(data_folder_path = file.path("testdata"),
                                 output_file_path = output_filename,
                                 parameter = "undiscovered"))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})
