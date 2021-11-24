test_that("create_empirical_island is correct for singleton endemic (stac 2)", {
  empirical_island <- create_empirical_island(
    total_time = 1,
    island_spec = create_test_island_spec(island_scenario = 55),
    mainland_clade = create_test_mainland_clade(mainland_scenario = 4),
    mainland_spec = 2)
  expected_empirical_island <- list(branching_times = c(1.0, 0.5),
                                stac = 2,
                                missing_species = 0)
  expect_equal(empirical_island, expected_empirical_island)
})

test_that("create_empirical_island is correct for singleton non-endemic
          (stac 1)", {
  empirical_island <- create_empirical_island(
    total_time = 1,
    island_spec = create_test_island_spec(island_scenario = 48),
    mainland_clade = create_test_mainland_clade(mainland_scenario = 21),
    mainland_spec = 1)
  expected_empirical_island <- list(branching_times = c(1.0, 0.99999),
                                    stac = 1,
                                    missing_species = 0)
  expect_equal(empirical_island, expected_empirical_island)
})

test_that("create_empirical_island is correct for singleton endemic (stac 5)", {
  empirical_island <- create_empirical_island(
    total_time = 1,
    island_spec = create_test_island_spec(island_scenario = 56),
    mainland_clade = create_test_mainland_clade(mainland_scenario = 3),
    mainland_spec = 1)
  expected_empirical_island <- list(branching_times = c(1.0, 0.99999),
                                    stac = 5,
                                    missing_species = 0)
  expect_equal(empirical_island, expected_empirical_island)
})

test_that("create_empirical_island is correct for clades (stac 6)", {
  empirical_island <- create_empirical_island(
    total_time = 1,
    island_spec = create_test_island_spec(island_scenario = 50),
    mainland_clade = create_test_mainland_clade(mainland_scenario = 3),
    mainland_spec = c(2, 3))
  expected_empirical_island <- list(branching_times = c(1.0, 0.99999, 0.25),
                                    stac = 6,
                                    missing_species = 0)
  expect_equal(empirical_island, expected_empirical_island)
})

test_that("create_empirical_island is correct for multiple colonists
          (stac 2)", {
  empirical_island <- create_empirical_island(
    total_time = 1,
    island_spec = create_test_island_spec(island_scenario = 57),
    mainland_clade = create_test_mainland_clade(mainland_scenario = 22),
    mainland_spec = c(3, 4))
  expected_empirical_island <- list(branching_times = c(1.0, 0.67, 0.60),
                                    stac = 2,
                                    missing_species = 0)
  expect_equal(empirical_island, expected_empirical_island)
})

test_that("create_empirical_island is correct for multiple colonists
          (stac 6)", {
  empirical_island <- create_empirical_island(
    total_time = 1,
    island_spec = create_test_island_spec(island_scenario = 58),
    mainland_clade = create_test_mainland_clade(mainland_scenario = 5),
    mainland_spec = c(1, 2))
  expected_empirical_island <- list(branching_times = c(1.0, 0.99999, 0.67),
                                    stac = 6,
                                    missing_species = 0)
  expect_equal(empirical_island, expected_empirical_island)
})
