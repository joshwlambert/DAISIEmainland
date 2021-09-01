#' Calculates the absolute difference in parameter estimates (cladogenesis,
#' extinction, carrying capacity, colonisation, and anagenesis) from the DAISIE
#' maximum likelihood model fitted to the ideal and empirical data sets
#' simulated from \code{sim_island_with_mainland}.
#'
#' @inheritParams default_params_doc
#'
#' @return A list of five numeric vectors
#' @author Joshua W. Lambert
calc_param_diffs <- function(ideal_ml,
                            empirical_ml) {

  testit::assert(length(ideal_ml) == length(empirical_ml))

  clado_abs_diff <- c()
  ext_abs_diff <- c()
  k_abs_diff <- c()
  immig_abs_diff <- c()
  ana_abs_diff <- c()

  for (i in seq_along(ideal_ml)) {
    clado_abs_diff[i] <-
      abs(ideal_ml[[i]]$lambda_c - empirical_ml[[i]]$lambda_c)
    ext_abs_diff[i] <-
      abs(ideal_ml[[i]]$mu - empirical_ml[[i]]$mu)
    k_abs_diff[i] <-
      abs(ideal_ml[[i]]$K - empirical_ml[[i]]$K)
    immig_abs_diff[i] <-
      abs(ideal_ml[[i]]$gamma - empirical_ml[[i]]$gamma)
    ana_abs_diff[i] <-
      abs(ideal_ml[[i]]$lambda_a - empirical_ml[[i]]$lambda_a)
  }

  param_abs_diff <- list(clado_abs_diff = clado_abs_diff,
                         ext_abs_diff = ext_abs_diff,
                         k_abs_diff = k_abs_diff,
                         immig_abs_diff = immig_abs_diff,
                         ana_abs_diff = ana_abs_diff)

  return(param_abs_diff)
}
