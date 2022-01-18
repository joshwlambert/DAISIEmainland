test_that("calc_sim_metrics runs silent without error", {
  analysis_results <- read_analysis_results(
    data_folder_path = file.path("testdata")
  )

  expect_silent(sim_metrics <- calc_sim_metrics(
    analysis_results = analysis_results,
    output_file_path = NULL
  ))

  expect_length(sim_metrics, 12)
  expect_equal(sim_metrics$ideal_mean_num_col, 34.2304347826)
  expect_equal(sim_metrics$ideal_max_num_col, 57)
  expect_equal(sim_metrics$ideal_min_num_col, 21)
  expect_equal(sim_metrics$ideal_mean_num_spec, 74.1239130435)
  expect_equal(sim_metrics$ideal_max_num_spec, 154)
  expect_equal(sim_metrics$ideal_min_num_spec, 32)
  expect_equal(sim_metrics$empirical_mean_num_col, 33.8586956522)
  expect_equal(sim_metrics$empirical_max_num_col, 56)
  expect_equal(sim_metrics$empirical_min_num_col, 21)
  expect_equal(sim_metrics$empirical_mean_num_spec, 74.1239130435)
  expect_equal(sim_metrics$empirical_max_num_spec, 154)
  expect_equal(sim_metrics$empirical_min_num_spec, 32)
})

test_that("calc_sim_metrics (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  analysis_results <- read_analysis_results(
    data_folder_path = file.path("testdata")
  )

  expect_silent(sim_metrics <- calc_sim_metrics(
    analysis_results = analysis_results,
    output_file_path = output_filename
  ))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})
