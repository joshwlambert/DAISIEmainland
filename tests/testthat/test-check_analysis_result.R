test_that("check_analysis_results is silent", {
  analysis_results_list <- read_analysis_results(
    data_folder_path = file.path("testdata")
  )
  expect_silent(lapply(analysis_results_list, check_analysis_result))
})

test_that("check_analysis_results fails correctly", {
  expect_error(
    check_analysis_result(analysis_result = "nonsense")
  )
})
