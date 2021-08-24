#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
calc_error <- function(path = "results") {

  result_files <- list.files(
    path = path,
    pattern = "param_set",
    full.names = TRUE
  )


  ks <- calc_col_ks(result_files = result_files)

  max_age_ratio <- calc_max_age_ratio(result_files)

  endemic_ratio <- calc_endemic_ratio(result_files)

  param_diffs <- calc_param_diffs(result_files = result_files)

  return(list(ks = ks,
              max_age_ratio = max_age_ratio,
              endemic_ratio = endemic_ratio,
              param_diffs = param_diffs))
}
