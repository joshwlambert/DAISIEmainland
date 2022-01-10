test_that("plot_sim_metrics (no save) runs silent without error", {
  analysis_results <- read_analysis_results(
    data_folder_path = file.path("testdata")
  )

  expect_silent(plot_sim_metrics(
    analysis_results = analysis_results,
    output_file_path = NULL
  ))
})

test_that("plot_sim_metrics (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  analysis_results <- read_analysis_results(
    data_folder_path = file.path("testdata")
  )

  expect_silent(plot_sim_metrics(
    analysis_results = analysis_results,
    output_file_path = output_filename
  ))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})
