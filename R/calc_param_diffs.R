#' Calculates the differences in parameter estimates (cladogenesis, extinction,
#' carrying capacity, colonisation, and anagenesis) from the DAISIE maximum
#' likelihood model fitted to the ideal and empirical data sets simulated from
#' `sim_island_with_mainland`.
#'
#' @inheritParams default_params_doc
#'
#' @return A list of five numeric vectors
#' @author Joshua W. Lambert
calc_param_diffs <- function(ideal_ml,
                             empirical_ml) {

  testit::assert(length(ideal_ml) == length(empirical_ml))

  clado_diffs <- c()
  ext_diffs <- c()
  k_diffs <- c()
  immig_diffs <- c()
  ana_diffs <- c()

  for (i in seq_along(ideal_ml)) {
    clado_diffs[i] <- ideal_ml[[i]]$lambda_c - empirical_ml[[i]]$lambda_c
    ext_diffs[i] <- ideal_ml[[i]]$mu - empirical_ml[[i]]$mu
    k_diffs[i] <- ideal_ml[[i]]$K - empirical_ml[[i]]$K
    immig_diffs[i] <- ideal_ml[[i]]$gamma - empirical_ml[[i]]$gamma
    ana_diffs[i] <- ideal_ml[[i]]$lambda_a - empirical_ml[[i]]$lambda_a
  }

  param_diffs <- list(clado_diffs = clado_diffs,
                      ext_diffs = ext_diffs,
                      k_diffs = k_diffs,
                      immig_diffs = immig_diffs,
                      ana_diffs = ana_diffs)

  return(param_diffs)
}
