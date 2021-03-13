args <- commandArgs(TRUE)
print(args)
data <- read.csv(file = "data/param_space.csv")
print(nrow(data))
island <- sim_island_with_mainland(
  total_time = data$total_time[args],
  m = data$m[args],
  island_pars = c(data$island_clado[args],
                  data$island_ex[args],
                  data$island_k[args],
                  data$island_immig[args],
                  data$island_ana[args]),
  mainland_ex = data$mainland_ex[args],
  mainland_sample_prob = data$mainland_sample_prob[args],
  replicates = data$replicates[args],
  verbose = FALSE)

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


