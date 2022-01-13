test_that("Code copied from email", {
  skip("Not yet")
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
  plot_empirical_multi_daisie_data(daisie_mainland_data$empirical_multi_daisie_data, replicate_index = 1)
  plot_ideal_multi_daisie_data(daisie_mainland_data$ideal_multi_daisie_data, replicate_index = 1)
})

test_that("Arguments 4 must have names", {
  skip("WIP, #42")
  set.seed(
    4,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_data <- sim_island_with_mainland(
    total_time = 1.0,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1
  )
  plot_daisie_data(daisie_data)
})

test_that("Arguments 2 must have names", {
  skip("WIP, #42")
  set.seed(
    3,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  total_time <- 1.0
  m <- 10
  mainland <- sim_mainland(
    total_time = total_time,
    m = m,
    mainland_ex = 2.0
  )
  island <- sim_island(
    total_time = total_time,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland[[1]],
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  )
  daisie_data <- format_to_daisie_data(
    island_replicates = island,
    total_time = total_time,
    m = m
  )
  expect_error(plot_daisie_data(daisie_data), "Argument 2 must have names")
  plot_daisie_data(daisie_data)
})

test_that("use", {
  skip("WIP, #42")
  set.seed(
    13,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  total_time <- 1.0
  m <- 10
  mainland <- sim_mainland(
    total_time = total_time,
    m = m,
    mainland_ex = 2.0
  )
  plot_mainland(mainland)
  mainland_clade <- mainland[[5]]
  plot_mainland_clade(mainland_clade)
  island <- sim_island(
    total_time = total_time,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  )

  daisie_data <- format_to_daisie_data(
    island_replicates = island,
    total_time = total_time,
    m = m
  )
  plot_daisie_data(daisie_data)
})

test_that("example from vignette, ideal is nor empirical", {
  skip("WIP, #42")
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )

  replicates <- 1

  daisie_data <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 100,
    island_pars = c(1, 1, 50, 0.1, 1),
    mainland_ex = 0.5,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = replicates,
    verbose = FALSE
  )
  plot_daisie_data(daisie_data)
})

test_that("search for trouble", {
  skip("WIP, #42")
  for (seed in seq_len(10)) {
    message(seed)
    set.seed(
      seed,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
    total_time <- 1.0
    m <- 10
    mainland <- sim_mainland(
      total_time = total_time,
      m = m,
      mainland_ex = 2.0
    )
    plot_mainland(mainland)
    mainland_clade <- mainland[[1]]
    plot_mainland_clade(mainland_clade)
    island <- sim_island(
      total_time = total_time,
      island_pars = c(1, 1, 10, 12, 1),
      mainland = mainland_clade,
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
    )

    daisie_data <- format_to_daisie_data(
      island_replicates = island,
      total_time = total_time,
      m = m
    )
    plot_daisie_data(daisie_data)
  }
})
