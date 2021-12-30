test_that("Figure A1", {
  skip("WIP, #44")
  skip("Re-create figure A1")
  # If the mainland species immigrates and does not go extinct or speciate,
  # and no events happen on the island, the island species is non-endemic (NE),
  # and the colonisation
  # time in the empirical data (gamma_E) is the same as the colonisation time
  # in the ideal data (gamma).
  # Empirical and ideal data are assigned stac 4.
  island_with_mainland <- sim_island_and_mainland(
    total_time = 100.0,
    m = 2,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 0.0,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered"
  )
  expect_equal(
    names(island_with_mainland),
    c("mainland", "island", "island_as_daisie_data")
  )
  island_with_mainland$mainland
  expect_equal(
    names(island_with_mainland$island),
    c("ideal_island", "empirical_island")
  )
  expect_equal(
    names(island_with_mainland$island_as_daisie_data),
    c("ideal_islands", "empirical_islands")
  )

  # Empty island
  island_spec_before <- create_test_island_spec(island_scenario = 0)

  island_spec_after <- update_state(
    timeval = 0.2,
    total_time = 1.0,
    possible_event = str_to_event("immigration"),
    max_spec_id = 1,
    mainland_spec = 1,
    island_spec = island_spec_before
  )

  expect_identical(
    island_spec_after$island_spec,
    create_test_island_spec(island_scenario = 1)
  )
})
