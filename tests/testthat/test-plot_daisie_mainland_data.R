test_that("No extant colonists", {
  skip("WIP, #59")
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 10,
    verbose = FALSE
  )

  plot_daisie_mainland_data(daisie_mainland_data, replicate_index = 1) # Probably calls the next two functions :-)
  # plot_empirical_multi_daisie_data(daisie_mainland_data$empirical_multi_daisie_data, replicate_index = 1)
  # plot_ideal_multi_daisie_data(daisie_mainland_data$ideal_multi_daisie_data, replicate_index = 1)
})

test_that("One colonist clade", {
  skip("WIP, #59")
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = FALSE
  )

  plot_daisie_mainland_data(daisie_mainland_data, replicate_index = 1) # Probably calls the next two functions :-)
  # plot_empirical_multi_daisie_data(daisie_mainland_data$empirical_multi_daisie_data, replicate_index = 1)
  # plot_ideal_multi_daisie_data(daisie_mainland_data$ideal_multi_daisie_data, replicate_index = 1)
})
