test_that("anagenetic species", {
  set.seed(
    4,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  total_time <- 1.0
  mainland <- sim_mainland(
    total_time = total_time,
    m = 10,
    mainland_ex = 1.0
  )
  mainland_clade <- mainland[[1]]
  island_tbl <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  plot_island_tbl(total_time = total_time, island_tbl = island_tbl)
})

test_that("anagenetic, cladogenetic, immigrant", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  total_time <- 1.0
  mainland <- sim_mainland(
    total_time = total_time,
    m = 10,
    mainland_ex = 1.0
  )
  mainland_clade <- mainland[[1]]
  island_tbl <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  plot_island_tbl(total_time = total_time, island_tbl = island_tbl)
})


test_that("one lonely immigrant", {
  set.seed(
    3,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  total_time <- 1.0
  mainland <- sim_mainland(
    total_time = total_time,
    m = 10,
    mainland_ex = 1.0
  )
  mainland_clade <- mainland[[1]]
  island_tbl <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  plot_island_tbl(total_time = total_time, island_tbl = island_tbl)
})


test_that("stac == 6", {
  skip("temp skip in refactor")
  # Endemic clade with unknown colonisation time, but with a maximum to this
  # colonisation time
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_scenario <- sample(1:22, size = 1)
  expect_equal(mainland_scenario, 21)
  mainland_clade <- create_test_mainland_clade(
    mainland_scenario = mainland_scenario
  )
  plot_mainland_clade(mainland_clade)
  island_tbl <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  plot_island_tbl(island)
})
