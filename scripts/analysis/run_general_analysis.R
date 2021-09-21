args <- commandArgs(TRUE)

args <- as.numeric(args)

data("general_param_space")

island_clado <- general_param_space$island_clado[args]
island_ex <- general_param_space$island_ex[args]
island_k <- general_param_space$island_k[args]
island_immig <- general_param_space$island_immig[args]
island_ana <- general_param_space$island_ana[args]
mainland_ex <- general_param_space$mainland_ex[args]
mainland_sample_prob <- general_param_space$mainland_sample_prob[args]

island <- DAISIEmainland::sim_island_with_mainland(
  total_time = general_param_space$total_time[args],
  m = general_param_space$m[args],
  island_pars = c(island_clado,
                  island_ex,
                  island_k,
                  island_immig,
                  island_ana),
  mainland_ex = mainland_ex,
  mainland_sample_prob = mainland_sample_prob,
  replicates = general_param_space$replicates[args],
  verbose = TRUE)

ideal_sim_metrics <- vector("list", general_param_space$replicates[args])
empirical_sim_metrics <- vector("list", general_param_space$replicates[args])
ideal_ml <- vector("list", general_param_space$replicates[args])
empirical_ml <- vector("list", general_param_space$replicates[args])

ideal_sim_metrics <- calc_sim_metrics(
  daisie_data = island$ideal_islands)
empirical_sim_metrics <- calc_sim_metrics(
  daisie_data = island$empirical_islands)

for (i in seq_len(general_param_space$replicates[args])) {

  ml_failure <- TRUE
  while (ml_failure) {
    ideal_ml[[i]] <- DAISIE::DAISIE_ML_CS(
      datalist = island$ideal_islands[[i]],
      initparsopt = c(island_clado,
                      island_ex,
                      island_k,
                      island_immig,
                      island_ana),
      idparsopt = 1:5,
      parsfix = NULL,
      idparsfix = NULL,
      ddmodel = 11,
      jitter = 1e-5)

    if (ideal_ml[[i]]$conv == -1) {
      ml_failure <- TRUE
      message("Likelihood optimisation failed retrying with new initial values")
      island_clado <- stats::runif(n = 1,
                                   min = island_clado / 2,
                                   max = island_clado * 2)
      island_ex <- stats::runif(n = 1,
                                min = island_ex / 2,
                                max = island_ex * 2)
      island_k <- stats::runif(n = 1,
                               min = island_k / 2,
                               max = island_k * 2)
      island_immig <- stats::runif(n = 1,
                                   min = island_immig / 2,
                                   max = island_immig * 2)
      island_ana <- stats::runif(n = 1,
                                 min = island_ana / 2,
                                 max = island_ana * 2)
    } else if (ideal_ml[[i]]$conv == 0) {
      ml_failure <- FALSE
    } else {
      stop("Convergence error in likelihood optimisation")
    }
  }

  ml_failure <- TRUE
  while (ml_failure) {
    empirical_ml[[i]] <- DAISIE::DAISIE_ML_CS(
      datalist = island$empirical_islands[[i]],
      initparsopt = c(island_clado,
                      island_ex,
                      island_k,
                      island_immig,
                      island_ana),
      idparsopt = 1:5,
      parsfix = NULL,
      idparsfix = NULL,
      ddmodel = 11,
      jitter = 1e-5)

    if (empirical_ml[[i]]$conv == -1) {
      ml_failure <- TRUE
      message("Likelihood optimisation failed retrying with new initial values")
      island_clado <- stats::runif(n = 1,
                                   min = island_clado / 2,
                                   max = island_clado * 2)
      island_ex <- stats::runif(n = 1,
                                min = island_ex / 2,
                                max = island_ex * 2)
      island_k <- stats::runif(n = 1,
                               min = island_k / 2,
                               max = island_k * 2)
      island_immig <- stats::runif(n = 1,
                                   min = island_immig / 2,
                                   max = island_immig * 2)
      island_ana <- stats::runif(n = 1,
                                 min = island_ana / 2,
                                 max = island_ana * 2)
    } else if (empirical_ml[[i]]$conv == 0) {
      ml_failure <- FALSE
    } else {
      stop("Convergence error in likelihood optimisation")
    }
  }
}

error <- DAISIEmainland::calc_error(
  daisie_data = island,
  ideal_ml = ideal_ml,
  empirical_ml = empirical_ml)

output <- list(
  island = island,
  ideal_ml = ideal_ml,
  empirical_ml = empirical_ml,
  ideal_sim_metrics = ideal_sim_metrics,
  empirical_sim_metrics = empirical_sim_metrics,
  error = error,
  sim_params = c(
    island_clado = island_clado,
    island_ex = island_ex,
    island_k = island_k,
    island_immig = island_immig,
    island_ana = island_ana,
    mainland_ex = mainland_ex,
    mainland_sample_prob = mainland_sample_prob)
)

output_name <- paste0("general_param_set_", args, ".rds")

output_folder <- file.path(getwd(), "results")

output_file_path <- file.path(output_folder, output_name)

saveRDS(object = output, file = output_file_path)

message("Finished")

