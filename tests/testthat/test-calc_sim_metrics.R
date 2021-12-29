test_that("calc_sim_metrics runs silent without error", {
  expect_silent(sim_metrics <- calc_sim_metrics(
    data_folder_path = file.path("testdata"),
    output_file_path = NULL))

  expect_length(sim_metrics, 12)
  expect_equal(sim_metrics$ideal_mean_num_col, 16.9369565217)
  expect_equal(sim_metrics$ideal_max_num_col, 30)
  expect_equal(sim_metrics$ideal_min_num_col, 6)
  expect_equal(sim_metrics$ideal_mean_num_spec, 36.6630434783)
  expect_equal(sim_metrics$ideal_max_num_spec, 87)
  expect_equal(sim_metrics$ideal_min_num_spec, 7)
  expect_equal(sim_metrics$empirical_mean_num_col, 16.7586956522)
  expect_equal(sim_metrics$empirical_max_num_col, 28)
  expect_equal(sim_metrics$empirical_min_num_col, 6)
  expect_equal(sim_metrics$empirical_mean_num_spec, 36.6630434783)
  expect_equal(sim_metrics$empirical_max_num_spec, 87)
  expect_equal(sim_metrics$empirical_min_num_spec, 7)
})

test_that("calc_sim_metrics (save) runs silent without error", {
  output_filename <- tempfile(
    pattern = "",
    tmpdir = tempdir(),
    fileext = ".png"
  )
  expect_false(file.exists(output_filename))

  expect_silent(sim_metrics <- calc_sim_metrics(
    data_folder_path = file.path("testdata"),
    output_file_path = output_filename))

  file.remove(output_filename)
  expect_false(file.exists(output_filename))
})
