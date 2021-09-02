#' Calculates the colonisation through time (CTT) statistic, max age ratio,
#' percentage of endemics, and differences in parameter estimates from DAISIE
#' maximum likelihood estimation on ideal and empirical data simulated from
#' sim_island_with_mainland
#'
#' @inheritParams default_params_doc
#'
#' @return List of error metrics
#' @export
#' @author Joshua W. Lambert
calc_error <- function(daisie_data,
                       ideal_ml,
                       empirical_ml) {

  delta_ctt <- calc_ctt(daisie_data = daisie_data)

  max_age_ratio <- calc_max_age_ratio(daisie_data = daisie_data)

  endemic_percent <- calc_endemic_percent(daisie_data = daisie_data)

  param_diffs <- calc_param_diffs(ideal_ml = ideal_ml,
                                  empirical_ml = empirical_ml)

  return(list(delta_ctt = delta_ctt,
              max_age_ratio = max_age_ratio,
              endemic_percent = endemic_percent,
              param_diffs = param_diffs))
}