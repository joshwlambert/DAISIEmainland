test_that("sim_island is silent and produces correct empty island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  island_tbl <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_clade = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  )
  expect_silent(check_island_tbl(island_tbl))
  expect_equal(nrow(island_tbl), 0)
})

test_that("sim_island is silent and produces correct non-empty island", {
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
  island_tbl <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  )
  expect_silent(check_island_tbl(island_tbl))
  expect_equal(nrow(island_tbl), 4)
})

test_that("colonisations", {
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  # Pick an interesting one
  mainland_clade <- create_test_mainland_clade(
    mainland_scenario = 20
  )
  island_tbl <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 12, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  )
  check_island_tbl(island_tbl)
  expect_equal(nrow(island_tbl), 5)
})
