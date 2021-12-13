test_that("Arguments 2 must have names", {
  skip("TODO, always seem dull")
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
  # plot_mainland(mainland)
  mainland_clade <- mainland[[1]]
  plot_mainland_clade(mainland_clade)
  island <- sim_island(
    total_time = total_time,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")

  plot_island(island)
  daisie_data <- format_to_daisie_data(
    island_replicates = island,
    total_time = total_time,
    m = m
  )
  plot_daisie_data(daisie_data)
})

test_that("use", {
  skip("All is dull")
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
    mainland_sample_type = "complete")

  plot_island(island)
  daisie_data <- format_to_daisie_data(
    island_replicates = island,
    total_time = total_time,
    m = m
  )
  plot_daisie_data(daisie_data)
})

test_that("example from vignette, ideal is nor empirical", {
  skip("All is dull")
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")

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
  skip("No trouble")
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
    # plot_mainland(mainland)
    mainland_clade <- mainland[[1]]
    plot_mainland_clade(mainland_clade)
    island <- sim_island(
      total_time = total_time,
      island_pars = c(1, 1, 10, 12, 1),
      mainland = mainland_clade,
      mainland_sample_prob = 1,
      mainland_sample_type = "complete")

    plot_island(island)
    daisie_data <- format_to_daisie_data(
      island_replicates = island,
      total_time = total_time,
      m = m
    )
    plot_daisie_data(daisie_data)
  }
})
