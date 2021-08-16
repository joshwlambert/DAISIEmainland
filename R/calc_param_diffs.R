calc_param_diff <- function(result_files) {

  mean_abs_diff <- list()
  for (i in seq_along(result_files)) {
    results <- readRDS(result_files[i])

    testit::assert(length(results$ideal_ml) == length(results$empirical_ml))

    clado_abs_diff <- c()
    ext_abs_diff <- c()
    k_abs_diff <- c()
    immig_abs_diff <- c()
    ana_abs_diff <- c()
    for (j in seq_along(results$ideal_ml)) {
      clado_abs_diff[j] <-
        abs(results$ideal_ml[[j]]$lambda_c - results$empirical_ml[[j]]$lambda_c)
      ext_abs_diff[j] <-
        abs(results$ideal_ml[[j]]$mu - results$empirical_ml[[j]]$mu)
      k_abs_diff[j] <-
        abs(results$ideal_ml[[j]]$K - results$empirical_ml[[j]]$K)
      immig_abs_diff[j] <-
        abs(results$ideal_ml[[j]]$gamma - results$empirical_ml[[j]]$gamma)
      ana_abs_diff[j] <-
        abs(results$ideal_ml[[j]]$lambda_a - results$empirical_ml[[j]]$lambda_a)
    }

    mean_clado_abs_diff <- mean(clado_abs_diff)
    mean_ext_abs_diff <- mean(ext_abs_diff)
    mean_k_abs_diff <- mean(k_abs_diff)
    mean_immig_abs_diff <- mean(immig_abs_diff)
    mean_ana_abs_diff <- mean(ana_abs_diff)

    param_set_abs_diff <- c(clado = mean_clado_abs_diff,
                            ext = mean_ext_abs_diff,
                            k = mean_k_abs_diff,
                            immig = mean_immig_abs_diff,
                            ana = mean_ana_abs_diff)

    mean_abs_diff[[i]] <- param_set_abs_diff
  }
  return(mean_abs_diff)
}
