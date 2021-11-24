test_that("calc_sim_metrics runs silent without error", {
  expect_silent(sim_metrics <- calc_sim_metrics(
    data_folder_path = file.path("testdata"),
    output_file_path = NULL))

  expect_length(sim_metrics, 12)
  expect_equal(sim_metrics$ideal_mean_num_col, 34.2456521739)
  expect_equal(sim_metrics$ideal_max_num_col, 50)
  expect_equal(sim_metrics$ideal_min_num_col, 16)
  expect_equal(sim_metrics$ideal_mean_num_spec, 75.1782608696)
  expect_equal(sim_metrics$ideal_max_num_spec, 150)
  expect_equal(sim_metrics$ideal_min_num_spec, 28)
  expect_equal(sim_metrics$empirical_mean_num_col, 33.8413043478)
  expect_equal(sim_metrics$empirical_max_num_col, 49)
  expect_equal(sim_metrics$empirical_min_num_col, 16)
  expect_equal(sim_metrics$empirical_mean_num_spec, 75.1782608696)
  expect_equal(sim_metrics$empirical_max_num_spec, 150)
  expect_equal(sim_metrics$empirical_min_num_spec, 28)
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
