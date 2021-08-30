#' Title
#'
#' @param ideal_ml stub
#' @param empirical_ml stub
#'
#' @return
#' @export
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

  mean_clado_abs_diff <- mean(clado_abs_diff)
  mean_ext_abs_diff <- mean(ext_abs_diff)
  mean_k_abs_diff <- mean(k_abs_diff)
  mean_immig_abs_diff <- mean(immig_abs_diff)
  mean_ana_abs_diff <- mean(ana_abs_diff)

  mean_abs_diff <- list(clado = mean_clado_abs_diff,
                        ext = mean_ext_abs_diff,
                        k = mean_k_abs_diff,
                        immig = mean_immig_abs_diff,
                        ana = mean_ana_abs_diff)

  return(mean_abs_diff)
}
