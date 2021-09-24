#' Calculates the difference in parameter estimates (cladogenesis, extinction,
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

  clado_diff <- c()
  ext_diff <- c()
  k_diff <- c()
  immig_diff <- c()
  ana_diff <- c()

  for (i in seq_along(ideal_ml)) {
    clado_diff[i] <- ideal_ml[[i]]$lambda_c - empirical_ml[[i]]$lambda_c
    ext_diff[i] <- ideal_ml[[i]]$mu - empirical_ml[[i]]$mu
    k_diff[i] <- ideal_ml[[i]]$K - empirical_ml[[i]]$K
    immig_diff[i] <- ideal_ml[[i]]$gamma - empirical_ml[[i]]$gamma
    ana_diff[i] <- ideal_ml[[i]]$lambda_a - empirical_ml[[i]]$lambda_a
  }

  param_diff <- list(clado_diff = clado_diff,
                     ext_diff = ext_diff,
                     k_diff = k_diff,
                     immig_diff = immig_diff,
                     ana_diff = ana_diff)

  return(param_diff)
}
