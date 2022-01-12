#' Calculates the ratio in parameter estimates (cladogenesis, extinction,
#' carrying capacity, colonisation, and anagenesis) from the DAISIE maximum
#' likelihood model fitted to the ideal and empirical data sets simulated from
#' `sim_island_with_mainland`.
#'
#' @inheritParams default_params_doc
#'
#' @return A list of five numeric vectors
#' @author Joshua W. Lambert
calc_param_ratios <- function(ideal_ml,
                              empirical_ml) {
  testit::assert(length(ideal_ml) == length(empirical_ml))

  clado_ratio <- c()
  ext_ratio <- c()
  k_ratio <- c()
  immig_ratio <- c()
  ana_ratio <- c()

  for (i in seq_along(ideal_ml)) {
    clado_ratio[i] <- ideal_ml[[i]]$lambda_c / empirical_ml[[i]]$lambda_c
    ext_ratio[i] <- ideal_ml[[i]]$mu / empirical_ml[[i]]$mu
    k_ratio[i] <- ideal_ml[[i]]$K / empirical_ml[[i]]$K
    immig_ratio[i] <- ideal_ml[[i]]$gamma / empirical_ml[[i]]$gamma
    ana_ratio[i] <- ideal_ml[[i]]$lambda_a / empirical_ml[[i]]$lambda_a
  }

  param_ratio <- list(
    clado_ratio = clado_ratio,
    ext_ratio = ext_ratio,
    k_ratio = k_ratio,
    immig_ratio = immig_ratio,
    ana_ratio = ana_ratio
  )

  return(param_ratio)
}
