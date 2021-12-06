test_that("no colonizations", {
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  plot_mainland_clade(mainland_clade)
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  ideal_island <- island$ideal_island
  plot_ideal_island(ideal_island = island$ideal_island)
})

test_that("use", {
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
  plot_mainland_clade(mainland_clade)
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  ideal_island <- island$ideal_island
  plot_ideal_island(ideal_island = island$ideal_island)
})

test_that("9 island species", {
  set.seed(
    6,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
  plot_mainland_clade(mainland_clade)
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  ideal_island <- island$ideal_island
  plot_ideal_island(ideal_island = ideal_island)
  # Endemic
  # Non_endemic
})

test_that("ideal data has 2, 3 and 4", {
  # In the ideal data only stac 2, 3 and 4 are assigned
  # because it is known when the species
  # colonises the island

  # Test more heavy if you want :-)
  for (seed in seq_len(2)) {
    message("seed: ", seed)
    set.seed(
      seed,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
    mainland_scenario <- sample(1:22, size = 1)
    message("mainland_scenario: ", mainland_scenario)
    mainland_clade <- create_test_mainland_clade(
      mainland_scenario = 22
    )
    plot_mainland_clade(mainland_clade)
    island <- sim_island(
      total_time = 1,
      island_pars = c(1, 1, 10, 1, 1),
      mainland = mainland_clade,
      mainland_sample_prob = 1,
      mainland_sample_type = "complete")
    ideal_island <- island$ideal_island
    stacs <- collect_ideal_island_stacs(ideal_island)
    if (length(stacs) == 1 && stacs == 0) next
    expect_true(all(stacs %in% c(2, 3, 4)))
  }
})
