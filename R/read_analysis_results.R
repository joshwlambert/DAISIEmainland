#' Reads all the the results from a directory, with path is specified in
#' `data_folder_path`.
#'
#' @inheritParams default_params_doc
#'
#' @return A list of results
#' @export
#'
#' @examples
#' \dontrun{
#' results <- DAISIEmainland::read_analysis_results(
#'   file.path("tests", "testthat", "testdata")
#' )
#' }
read_analysis_results <- function(data_folder_path) {
  # load all the files from the path
  files <- list.files(data_folder_path)
  if (length(files) == 0) {
    stop("No results are in the results directory")
  } else {
    file_paths <- as.list(paste0(data_folder_path, "/", files))
    analysis_results_list <- lapply(file_paths, readRDS)
  }
  # check whether the files loaded are the correct format
  lapply(analysis_results_list, check_analysis_result)
  analysis_results_list
}
