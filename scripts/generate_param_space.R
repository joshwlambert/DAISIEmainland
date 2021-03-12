param_space <- expand.grid(total_time = c(5),
                           m = c(100, 500, 1000),
                           island_clado = c(0.5, 1),
                           island_ex = c(0.5, 1),
                           island_k = c(0.5, 1),
                           island_immig = c(0.01),
                           island_ana = c(10, Inf),
                           mainland_ex = c(0, 0.1, 0.3, 0.5),
                           mainland_sample_prob = c(0.7, 0.8, 0.9, 1.0),
                           replicates = c(1000))

write.csv(x = param_space, file = "data/param_space.csv", row.names = FALSE)
