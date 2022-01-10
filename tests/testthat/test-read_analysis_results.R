test_that("read_analysis_results produces correct output", {
  analysis_results_list <- read_analysis_results(
    data_folder_path = file.path("testdata")
  )
  expect_true(is.list(analysis_results_list))
  expect_length(analysis_results_list, 46)
})

test_that("read_analysis_results fails correctly", {
  expect_error(
    read_analysis_results(
      data_folder_path = "nonsense"
    ),
    "No results are in the results directory"
  )
})
