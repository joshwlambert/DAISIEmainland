test_that("plot_k_estimates mainland_ex (no save) runs silent without error", {
  analysis_results <- read_analysis_results(
    data_folder_path = file.path("testdata")
  )

  expect_silent(plot_k_estimates(
    analysis_results = analysis_results,
    output_file_path = NULL,
    parameter = "mainland_ex",
    num_breaks = 4,
    signif = 2,
    scientific = FALSE
  ))
})

test_that("plot_k_estimates unsampled (no save) runs silent without error", {
  analysis_results <- read_analysis_results(
    data_folder_path = file.path("testdata")
  )

  expect_silent(plot_k_estimates(
    analysis_results = analysis_results,
    output_file_path = NULL,
    parameter = "unsampled",
    num_breaks = 4,
    signif = 2,
    scientific = FALSE
  ))
})

test_that("plot_k_estimates undiscovered (no save) runs silent without error", {
  analysis_results <- read_analysis_results(
    data_folder_path = file.path("testdata")
  )

  expect_silent(plot_k_estimates(
    analysis_results = analysis_results,
    output_file_path = NULL,
    parameter = "undiscovered",
    num_breaks = 4,
    signif = 2,
    scientific = FALSE
  ))
})

test_that("plot_k_estimates mainland_ex (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  analysis_results <- read_analysis_results(
    data_folder_path = file.path("testdata")
  )

  expect_silent(plot_k_estimates(
    analysis_results = analysis_results,
    output_file_path = output_filename,
    parameter = "mainland_ex",
    num_breaks = 4,
    signif = 2,
    scientific = FALSE
  ))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})

test_that("plot_k_estimates unsampled (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  analysis_results <- read_analysis_results(
    data_folder_path = file.path("testdata")
  )

  expect_silent(plot_k_estimates(
    analysis_results = analysis_results,
    output_file_path = output_filename,
    parameter = "unsampled",
    num_breaks = 4,
    signif = 2,
    scientific = FALSE
  ))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})

test_that("plot_k_estimates undiscovered (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  analysis_results <- read_analysis_results(
    data_folder_path = file.path("testdata")
  )

  expect_silent(plot_k_estimates(
    analysis_results = analysis_results,
    output_file_path = output_filename,
    parameter = "undiscovered",
    num_breaks = 4,
    signif = 2,
    scientific = FALSE
  ))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})
