#' Calculates the ...
#'
#' @param daisie_data stub
#' @param ideal_ml stub
#' @param empirical_ml stub
#'
#' @return
#' @export
#' @author Joshua W. Lambert
calc_error <- function(daisie_data,
                       ideal_ml,
                       empirical_ml) {

  delta_ctt <- calc_ctt(daisie_data = daisie_data)

  max_age_ratio <- calc_max_age_ratio(daisie_data = daisie_data)

  endemic_ratio <- calc_endemic_ratio(daisie_data = daisie_data)

  param_diffs <- calc_param_diffs(ideal_ml = ideal_ml,
                                  empirical_ml = empirical_ml)

  return(list(delta_ctt = delta_ctt,
              max_age_ratio = max_age_ratio,
              endemic_ratio = endemic_ratio,
              param_diffs = param_diffs))
}
