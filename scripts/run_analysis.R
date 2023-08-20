args <- commandArgs(TRUE)

args <- as.numeric(args)

data("param_space")

set.seed(
  param_space$seed[args],
  kind = "Mersenne-Twister",
  normal.kind = "Inversion",
  sample.kind = "Rejection"
)

island_clado <- param_space$island_clado[args]
island_ex <- param_space$island_ex[args]
island_k <- param_space$island_k[args]
island_immig <- param_space$island_immig[args]
island_ana <- param_space$island_ana[args]
mainland_ex <- param_space$mainland_ex[args]
mainland_sample_prob <- param_space$mainland_sample_prob[args]
mainland_sample_type <- param_space$mainland_sample_type[args]

daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
  total_time = param_space$total_time[args],
  m = param_space$m[args],
  island_pars = c(island_clado,
                  island_ex,
                  island_k,
                  island_immig,
                  island_ana),
  mainland_ex = mainland_ex,
  mainland_sample_prob = mainland_sample_prob,
  mainland_sample_type = mainland_sample_type,
  replicates = param_space$replicates[args],
  verbose = TRUE)

ideal_sim_metrics <- vector("list", param_space$replicates[args])
empirical_sim_metrics <- vector("list", param_space$replicates[args])
ideal_ml <- vector("list", param_space$replicates[args])
empirical_ml <- vector("list", param_space$replicates[args])

ideal_sim_num_spec <- DAISIEmainland::calc_num_spec(
  multi_daisie_data = daisie_mainland_data$ideal_multi_daisie_data
)
ideal_sim_num_col <- DAISIEmainland::calc_num_col(
  multi_daisie_data = daisie_mainland_data$ideal_multi_daisie_data
)
ideal_sim_metrics <- list(
  ideal_sim_num_spec = ideal_sim_num_spec,
  ideal_sim_num_col = ideal_sim_num_col
)
empirical_sim_num_spec <- DAISIEmainland::calc_num_spec(
  multi_daisie_data = daisie_mainland_data$empirical_multi_daisie_data
)
empirical_sim_num_col <- DAISIEmainland::calc_num_col(
  multi_daisie_data = daisie_mainland_data$empirical_multi_daisie_data
)
empirical_sim_metrics <- list(
  empirical_sim_num_spec = empirical_sim_num_spec,
  empirical_sim_num_col = empirical_sim_num_col
)

message("Number of likelihood integration steps permitted:")
DAISIE::DAISIE_CS_max_steps(1e8)

endemics <- DAISIEmainland:::calc_endemic_percent(
  daisie_mainland_data = daisie_mainland_data
)

for (i in seq_len(param_space$replicates[args])) {
  ml_failure <- TRUE
  while (ml_failure) {
    recols <- DAISIEmainland::any_recols(
      daisie_data = daisie_mainland_data$ideal_multi_daisie_data[[i]])
    optim_ana <- endemics$ideal_endemic_percent[i] != 100 || recols
    if (optim_ana) {
      ideal_ml[[i]] <- DAISIE::DAISIE_ML_CS(
        datalist = daisie_mainland_data$ideal_multi_daisie_data[[i]],
        initparsopt = c(island_clado,
                        island_ex,
                        island_k,
                        island_immig,
                        island_ana),
        idparsopt = 1:5,
        parsfix = NULL,
        idparsfix = NULL,
        ddmodel = 11,
        methode = "odeint::runge_kutta_fehlberg78",
        optimmethod = "simplex",
        jitter = 1e-5)
    } else {
      fix_ana_zero <- DAISIEmainland::all_endemic_clades(
        daisie_data = daisie_mainland_data$ideal_multi_daisie_data[[i]])
      if (fix_ana_zero) {
        ideal_ml[[i]] <- DAISIE::DAISIE_ML_CS(
          datalist = daisie_mainland_data$ideal_multi_daisie_data[[i]],
          initparsopt = c(island_clado,
                          island_ex,
                          island_k,
                          island_immig),
          idparsopt = 1:4,
          parsfix = 0,
          idparsfix = 5,
          ddmodel = 11,
          methode = "odeint::runge_kutta_fehlberg78",
          optimmethod = "simplex",
          jitter = 1e-5)
      } else {
        ideal_ml[[i]] <- DAISIE::DAISIE_ML_CS(
          datalist = daisie_mainland_data$ideal_multi_daisie_data[[i]],
          initparsopt = c(island_clado,
                          island_ex,
                          island_k,
                          island_immig),
          idparsopt = 1:4,
          parsfix = 100,
          idparsfix = 5,
          ddmodel = 11,
          methode = "odeint::runge_kutta_fehlberg78",
          optimmethod = "simplex",
          jitter = 1e-5)
      }
    }
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
    recols <- DAISIEmainland::any_recols(
      daisie_data = daisie_mainland_data$empirical_multi_daisie_data[[i]])
    optim_ana <- endemics$empirical_endemic_percent[i] != 100 || recols
    if (optim_ana) {
      empirical_ml[[i]] <- DAISIE::DAISIE_ML_CS(
        datalist = daisie_mainland_data$empirical_multi_daisie_data[[i]],
        initparsopt = c(island_clado,
                        island_ex,
                        island_k,
                        island_immig,
                        island_ana),
        idparsopt = 1:5,
        parsfix = NULL,
        idparsfix = NULL,
        ddmodel = 11,
        methode = "odeint::runge_kutta_fehlberg78",
        optimmethod = "simplex",
        jitter = 1e-5)
    } else {
      fix_ana_zero <- DAISIEmainland::all_endemic_clades(
        daisie_data = daisie_mainland_data$empirical_multi_daisie_data[[i]])
      if (fix_ana_zero) {
        empirical_ml[[i]] <- DAISIE::DAISIE_ML_CS(
          datalist = daisie_mainland_data$empirical_multi_daisie_data[[i]],
          initparsopt = c(island_clado,
                          island_ex,
                          island_k,
                          island_immig),
          idparsopt = 1:4,
          parsfix = 0,
          idparsfix = 5,
          ddmodel = 11,
          methode = "odeint::runge_kutta_fehlberg78",
          optimmethod = "simplex",
          jitter = 1e-5)
      } else {
        empirical_ml[[i]] <- DAISIE::DAISIE_ML_CS(
          datalist = daisie_mainland_data$empirical_multi_daisie_data[[i]],
          initparsopt = c(island_clado,
                          island_ex,
                          island_k,
                          island_immig),
          idparsopt = 1:4,
          parsfix = 100,
          idparsfix = 5,
          ddmodel = 11,
          methode = "odeint::runge_kutta_fehlberg78",
          optimmethod = "simplex",
          jitter = 1e-5)
      }
    }
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
  daisie_mainland_data = daisie_mainland_data,
  ideal_ml = ideal_ml,
  empirical_ml = empirical_ml)

output <- list(
  daisie_mainland_data = daisie_mainland_data,
  ideal_ml = ideal_ml,
  empirical_ml = empirical_ml,
  ideal_sim_metrics = ideal_sim_metrics,
  empirical_sim_metrics = empirical_sim_metrics,
  error = error,
  sim_params = list(
    island_clado = param_space$island_clado[args],
    island_ex = param_space$island_ex[args],
    island_k = param_space$island_k[args],
    island_immig = param_space$island_immig[args],
    island_ana = param_space$island_ana[args],
    mainland_ex = param_space$mainland_ex[args],
    mainland_sample_prob = param_space$mainland_sample_prob[args],
    mainland_sample_type = param_space$mainland_sample_type[args])
)

output_name <- paste0("param_set_", args, ".rds")

output_folder <- file.path("results")

output_file_path <- file.path(output_folder, output_name)

saveRDS(object = output, file = output_file_path)

message("Finished")
