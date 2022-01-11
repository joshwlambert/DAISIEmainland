#' Checks whether the analysis results read by `read_analysis_results` is valid.
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#' @examples
#' \dontrun{
#' analysis_results <- DAISIEmainland::read_analysis_results(
#'   file.path("tests", "testthat", "testdata")
#' )
#' analysis_result <- analysis_results[[1]]
#' check_analysis_result(analysis_result)
#' }
#' @author Joshua W. Lambert
#' @export
check_analysis_result <- function(analysis_result) {
  testit::assert(identical(
    names(analysis_result),
    c("daisie_mainland_data", "ideal_ml", "empirical_ml", "ideal_sim_metrics",
      "empirical_sim_metrics", "error", "sim_params")
    ))
  testit::assert(identical(length(analysis_result), 7L))
  check_daisie_mainland_data(analysis_result$daisie_mainland_data)
  testit::assert(all(sapply(analysis_result$ideal_ml, is.data.frame)))
  testit::assert(all(sapply(analysis_result$ideal_ml, function(x) {
    identical(
      names(x),
      c("lambda_c", "mu", "K","gamma", "lambda_a", "loglik", "df", "conv"))
  })))
  testit::assert(all(sapply(analysis_result$empirical_ml, is.data.frame)))
  testit::assert(all(sapply(analysis_result$empirical_ml, function(x) {
    identical(
      names(x),
      c("lambda_c", "mu", "K","gamma", "lambda_a", "loglik", "df", "conv"))
  })))
  testit::assert(identical(
    names(analysis_result$ideal_sim_metrics),
    c("ideal_sim_num_spec", "ideal_sim_num_col")
  ))
  testit::assert(all(sapply(analysis_result$ideal_sim_metrics, is.numeric)))
  testit::assert(identical(
    names(analysis_result$empirical_sim_metrics),
    c("empirical_sim_num_spec", "empirical_sim_num_col")
  ))
  testit::assert(all(sapply(analysis_result$empirical_sim_metrics, is.numeric)))
  testit::assert(identical(
    names(analysis_result$error),
    c("delta_ctt", "max_age_percent", "endemic_percent",
      "param_diffs","param_ratios")
  ))
  testit::assert(all(unlist(sapply(analysis_result$error, function(x) {
    if (is.list(x)) {
      sapply(x, is.numeric)
    } else {
      is.numeric(x)
    }
  }))))
  testit::assert(identical(
    names(analysis_result$sim_params),
    c("island_clado", "island_ex", "island_k", "island_immig", "island_ana",
      "mainland_ex", "mainland_sample_prob", "mainland_sample_type")
  ))
  testit::assert(all(sapply(analysis_result$sim_params, function(x) {
    is.character(x) || is.numeric(x)
  })))
  invisible(analysis_result)
}
