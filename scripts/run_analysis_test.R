args <- commandArgs(TRUE)
print(args)
args <- as.numeric(args)
print(args)
data <- read.csv(file = "data/param_space.csv")
print(nrow(data))
replicates <- 10
island <- DAISIEmainland::sim_island_with_mainland(
  total_time = data$total_time[args],
  m = data$m[args],
  island_pars = c(data$island_clado[args],
                  data$island_ex[args],
                  data$island_k[args],
                  data$island_immig[args],
                  data$island_ana[args]),
  mainland_ex = data$mainland_ex[args],
  mainland_sample_prob = data$mainland_sample_prob[args],
  replicates = replicates,
  verbose = TRUE)

ideal_ml <- vector("list", replicates)
empirical_ml <- vector("list", replicates)

for (i in seq_len(replicates)) {
  ideal_ml[[i]] <- DAISIE::DAISIE_ML_CS(
    datalist = island$ideal_islands[[i]],
    initparsopt = c(data$island_clado[args],
                    data$island_ex[args],
                    data$island_k[args],
                    data$island_immig[args],
                    data$island_ana[args]),
    idparsopt = 1:5,
    parsfix = NULL,
    idparsfix = NULL,
    ddmodel = 11,
    jitter = 1e-5)

  empirical_ml[[i]] <- DAISIE::DAISIE_ML_CS(
    datalist = island$reality_islands[[i]],
    initparsopt = c(data$island_clado[args],
                    data$island_ex[args],
                    data$island_k[args],
                    data$island_immig[args],
                    data$island_ana[args]),
    idparsopt = 1:5,
    parsfix = NULL,
    idparsfix = NULL,
    ddmodel = 11,
    jitter = 1e-5)
}

output <- list(
  island = island,
  ideal_ml = ideal_ml,
  empirical_ml = empirical_ml)

output_name <- paste0("param_set_", args)

output_folder <- file.path(
  getwd(),
  "results")

output_file_path <- file.path(output_folder, output_name)

save(output, file = output_file_path)

