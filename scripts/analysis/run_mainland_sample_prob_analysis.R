args <- commandArgs(TRUE)

args <- as.numeric(args)

data("mainland_sample_prob_param_space")

island <- DAISIEmainland::sim_island_with_mainland(
  total_time = mainland_sample_prob_param_space$total_time[args],
  m = mainland_sample_prob_param_space$m[args],
  island_pars = c(mainland_sample_prob_param_space$island_clado[args],
                  mainland_sample_prob_param_space$island_ex[args],
                  mainland_sample_prob_param_space$island_k[args],
                  mainland_sample_prob_param_space$island_immig[args],
                  mainland_sample_prob_param_space$island_ana[args]),
  mainland_ex = mainland_sample_prob_param_space$mainland_ex[args],
  mainland_sample_prob = mainland_sample_prob_param_space$mainland_sample_prob[args],
  replicates = mainland_sample_prob_param_space$replicates[args],
  verbose = TRUE)

ideal_ml <- vector("list", mainland_sample_prob_param_space$replicates[args])
empirical_ml <- vector("list", mainland_sample_prob_param_space$replicates[args])

for (i in seq_len(mainland_sample_prob_param_space$replicates[args])) {
  message("Number of clades ", length(island$ideal_islands[[i]]) - 1)

  if (length(island$ideal_islands[[i]]) > 1) {
    message("Number of species in each clade")
    for (j in 2:length(island$ideal_islands[[i]])) {
      message(length(island$ideal_islands[[i]][[j]]$branching_times) - 1)
    }
  }

  ml_failure <- TRUE
  while (ml_failure) {
    ideal_ml[[i]] <- DAISIE::DAISIE_ML_CS(
      datalist = island$ideal_islands[[i]],
      initparsopt = c(mainland_sample_prob_param_space$island_clado[args],
                      mainland_sample_prob_param_space$island_ex[args],
                      mainland_sample_prob_param_space$island_k[args],
                      mainland_sample_prob_param_space$island_immig[args],
                      mainland_sample_prob_param_space$island_ana[args]),
      idparsopt = 1:5,
      parsfix = NULL,
      idparsfix = NULL,
      ddmodel = 11,
      jitter = 1e-5)

    if (ideal_ml[[i]]$conv == -1) {
      ml_failure <- TRUE
      message("Likelihood optimisation failed retrying")
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
      initparsopt = c(mainland_sample_prob_param_space$island_clado[args],
                      mainland_sample_prob_param_space$island_ex[args],
                      mainland_sample_prob_param_space$island_k[args],
                      mainland_sample_prob_param_space$island_immig[args],
                      mainland_sample_prob_param_space$island_ana[args]),
      idparsopt = 1:5,
      parsfix = NULL,
      idparsfix = NULL,
      ddmodel = 11,
      jitter = 1e-5)

    if (empirical_ml[[i]]$conv == -1) {
      ml_failure <- TRUE
      message("Likelihood optimisation failed retrying")
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
  error = error,
  sim_params = c(
    island_clado = mainland_sample_prob_param_space$island_clado[args],
    island_ex = mainland_sample_prob_param_space$island_ex[args],
    island_k = mainland_sample_prob_param_space$island_k[args],
    island_immig = mainland_sample_prob_param_space$island_immig[args],
    island_ana = mainland_sample_prob_param_space$island_ana[args],
    mainland_ex = mainland_sample_prob_param_space$mainland_ex[args],
    mainland_sample_prob = mainland_sample_prob_param_space$mainland_sample_prob[args])
)

output_name <- paste0("mainland_sample_prob_param_set_", args, ".rds")

output_folder <- file.path(getwd(), "results")

output_file_path <- file.path(output_folder, output_name)

saveRDS(object = output, file = output_file_path)

message("Finished")

