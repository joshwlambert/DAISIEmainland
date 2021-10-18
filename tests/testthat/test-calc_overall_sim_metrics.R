test_that("calc_overall_sim_metrics runs silent without error", {
  expect_silent(overall_sim_metrics <- calc_overall_sim_metrics(
    data_folder_path = file.path("testdata"),
    output_file_path = NULL))

  expect_length(overall_sim_metrics, 12)
  expect_equal(overall_sim_metrics$overall_ideal_mean_num_col, 34.29)
  expect_equal(overall_sim_metrics$overall_ideal_max_num_col, 51)
  expect_equal(overall_sim_metrics$overall_ideal_min_num_col, 21)
  expect_equal(overall_sim_metrics$overall_ideal_mean_num_spec, 74.15)
  expect_equal(overall_sim_metrics$overall_ideal_max_num_spec, 129)
  expect_equal(overall_sim_metrics$overall_ideal_min_num_spec, 32)
  expect_equal(overall_sim_metrics$overall_empirical_mean_num_col, 34.28)
  expect_equal(overall_sim_metrics$overall_empirical_max_num_col, 51)
  expect_equal(overall_sim_metrics$overall_empirical_min_num_col, 21)
  expect_equal(overall_sim_metrics$overall_empirical_mean_num_spec, 74.15)
  expect_equal(overall_sim_metrics$overall_empirical_max_num_spec, 129)
  expect_equal(overall_sim_metrics$overall_empirical_min_num_spec, 32)
})

test_that("calc_overall_sim_metrics (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(overall_sim_metrics <- calc_overall_sim_metrics(
    data_folder_path = file.path("testdata"),
    output_file_path = output_filename))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})
