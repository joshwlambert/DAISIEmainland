test_that("interesting island, with recolonisations", {
  skip("WIP, #42")
  set.seed(
    9,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland <- sim_mainland(
    total_time = 10,
    m = 10,
    mainland_ex = 1.0
  )

  # Clade goes extinct, but after island age
  mainland_clade <- mainland[[1]]

  plot_mainland_clade(mainland_clade)
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")

  plot_island_tbl(island)
  daisie_data <- format_to_daisie_data(
    island_replicates = island,
    total_time = 1,
    m = m
  )
  empirical_daisie_data <- daisie_data$empirical_islands
  check_empirical_daisie_data(empirical_daisie_data)
  empirical_daisie_data_to_tables(empirical_daisie_data)
})

test_that("search for non-dull scenario", {
  skip("WIP, #42")
  for (seed in seq_len(1000)) {
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

    plot_island_tbl(island)
    daisie_data <- format_to_daisie_data(
      island_replicates = island,
      total_time = total_time,
      m = m
    )
    empirical_daisie_data <- daisie_data$empirical_islands

    is_dull <- function(empirical_daisie_data) {
      length(empirical_daisie_data) == 2 &&
      length(empirical_daisie_data[[1]]) == 1 &&
      length(empirical_daisie_data[[1]][[1]]) == 2 &&
      empirical_daisie_data[[1]][[1]]$island_age == total_time &&
      empirical_daisie_data[[1]][[1]]$not_present == 0 &&
      length(empirical_daisie_data[[2]]) == 1 &&
      length(empirical_daisie_data[[2]][[1]]) == 2 &&
      empirical_daisie_data[[2]][[1]]$island_age == total_time &&
      empirical_daisie_data[[2]][[1]]$not_present == 0
    }
    expect_true(is_dull(empirical_daisie_data))
    # expect_silent(
    #   empirical_daisie_data_to_tables(empirical_daisie_data)
    # )
  }
})
