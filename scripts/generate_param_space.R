param_space <- expand.grid(c(5),
                           c(100, 500, 1000),
                           c(0.5, 1),
                           c(0.5, 1),
                           c(0.5, 1),
                           c(0.01),
                           c(10, Inf),
                           c(0, 0.1, 0.3, 0.5),
                           c(0.7, 0.8, 0.9, 1.0),
                           c(1000))

colnames(param_space) <- c("time",
                           "m",
                           "island_clado",
                           "island_ex",
                           "island_k",
                           "island_immig",
                           "island_ana",
                           "mainland_ex",
                           "mainland_sample_prob",
                           "replicates")

write.csv(x = param_space, file = "data/param_space.csv", row.names = FALSE)
