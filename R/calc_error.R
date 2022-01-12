#' Calculates the colonisation through time (CTT) statistic, percentage of max
#' age colonists, percentage of endemics, and differences in parameter estimates
#' from DAISIE maximum likelihood estimation on ideal and empirical data
#' simulated from sim_island_with_mainland
#'
#' @inheritParams default_params_doc
#'
#' @return List of error metrics
#' @export
#' @author Joshua W. Lambert
calc_error <- function(daisie_mainland_data,
                       ideal_ml,
                       empirical_ml) {
  delta_ctt <- calc_ctt(daisie_mainland_data = daisie_mainland_data)

  max_age_percent <- calc_max_age_percent(
    daisie_mainland_data = daisie_mainland_data
  )

  endemic_percent <- calc_endemic_percent(
    daisie_mainland_data = daisie_mainland_data
  )

  param_diffs <- calc_param_diffs(
    ideal_ml = ideal_ml,
    empirical_ml = empirical_ml
  )

  param_ratios <- calc_param_ratios(
    ideal_ml = ideal_ml,
    empirical_ml = empirical_ml
  )

  return(list(
    delta_ctt = delta_ctt,
    max_age_percent = max_age_percent,
    endemic_percent = endemic_percent,
    param_diffs = param_diffs,
    param_ratios = param_ratios
  ))
}
