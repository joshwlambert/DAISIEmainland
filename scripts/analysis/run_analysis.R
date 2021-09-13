args <- commandArgs(TRUE)

args <- as.numeric(args)

data("param_space")

island <- DAISIEmainland::sim_island_with_mainland(
  total_time = param_space$total_time[args],
  m = param_space$m[args],
  island_pars = c(param_space$island_clado[args],
                  param_space$island_ex[args],
                  param_space$island_k[args],
                  param_space$island_immig[args],
                  param_space$island_ana[args]),
  mainland_ex = param_space$mainland_ex[args],
  mainland_sample_prob = param_space$mainland_sample_prob[args],
  replicates = param_space$replicates[args],
  verbose = TRUE)

ideal_ml <- vector("list", param_space$replicates[args])
empirical_ml <- vector("list", param_space$replicates[args])

for (i in seq_len(param_space$replicates[args])) {
  message("Number of clades ", length(island$ideal_islands[[i]]) - 1)

  if (length(island$ideal_islands[[i]]) > 1) {
    message("Number of species in each clade")
    for (j in 2:length(island$ideal_islands[[i]])) {
      message(length(island$ideal_islands[[i]][[j]]$branching_times) - 1)
    }
  }

  ideal_ml[[i]] <- DAISIE::DAISIE_ML_CS(
    datalist = island$ideal_islands[[i]],
    initparsopt = c(param_space$island_clado[args],
                    param_space$island_ex[args],
                    param_space$island_k[args],
                    param_space$island_immig[args],
                    param_space$island_ana[args]),
    idparsopt = 1:5,
    parsfix = NULL,
    idparsfix = NULL,
    ddmodel = 11,
    jitter = 1e-5)

  empirical_ml[[i]] <- DAISIE::DAISIE_ML_CS(
    datalist = island$empirical_islands[[i]],
    initparsopt = c(param_space$island_clado[args],
                    param_space$island_ex[args],
                    param_space$island_k[args],
                    param_space$island_immig[args],
                    param_space$island_ana[args]),
    idparsopt = 1:5,
    parsfix = NULL,
    idparsfix = NULL,
    ddmodel = 11,
    jitter = 1e-5)
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
    param_space$island_clado[args],
    param_space$island_ex[args],
    param_space$island_k[args],
    param_space$island_immig[args],
    param_space$island_ana[args],
    mainland_ex = param_space$mainland_ex[args],
    mainland_sample_prob = param_space$mainland_sample_prob[args])
)

output_name <- paste0("param_set_", args, ".rds")

output_folder <- file.path(getwd(), "results")

output_file_path <- file.path(output_folder, output_name)

saveRDS(object = output, file = output_file_path)

message("Finished")

