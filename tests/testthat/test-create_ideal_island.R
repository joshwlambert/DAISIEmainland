test_that("create_ideal_island is correct for singleton nonendemic (stac 4)", {
  island_spec <- create_test_island_spec(island_scenario = 48)
  ideal_island <- create_ideal_island(
    total_time = 1,
    island_spec = island_spec)
  expected_ideal_island <- list(branching_times = c(1.0, 0.5),
                                stac = 4,
                                missing_species = 0)
  expect_equal(ideal_island, expected_ideal_island)
})

test_that("create_ideal_island is correct for singleton endemic (stac 2)", {
  island_spec <- create_test_island_spec(island_scenario = 49)
  ideal_island <- create_ideal_island(
    total_time = 1,
    island_spec = island_spec)
  expected_ideal_island <- list(branching_times = c(1.0, 0.5),
                                stac = 2,
                                missing_species = 0)
  expect_equal(ideal_island, expected_ideal_island)
})

test_that("create_ideal_island is correct for endemic clade (stac 2)", {
  island_spec <- create_test_island_spec(island_scenario = 50)
  ideal_island <- create_ideal_island(
    total_time = 1,
    island_spec = island_spec)
  expected_ideal_island <- list(branching_times = c(1.0, 0.5, 0.25),
                                stac = 2,
                                missing_species = 0)
  expect_equal(ideal_island, expected_ideal_island)
})

test_that("create_ideal_island is correct for anagenetic and non-endemic
          recolonisation (stac 3)", {
  island_spec <- create_test_island_spec(island_scenario = 51)
  ideal_island <- create_ideal_island(
    total_time = 1,
    island_spec = island_spec)
  expected_ideal_island <- list(branching_times = c(1.0, 0.5),
                                stac = 3,
                                missing_species = 0,
                                all_colonisations = list(
                                  list(event_times = c(1.0, 0.5),
                                       species_type = "A"),
                                  list(event_times = c(1.0, 0.3),
                                       species_type = "I")
                                ))
  expect_equal(ideal_island, expected_ideal_island)
})

test_that("create_ideal_island is correct for cladogenetic and non-endemic
          recolonisation (stac 3)", {
  island_spec <- create_test_island_spec(island_scenario = 52)
  ideal_island <- create_ideal_island(
    total_time = 1,
    island_spec = island_spec)
  expected_ideal_island <- list(branching_times = c(1.0, 0.5, 0.4),
                                stac = 3,
                                missing_species = 0,
                                all_colonisations = list(
                                  list(event_times = c(1.0, 0.5, 0.4),
                                       species_type = "C"),
                                  list(event_times = c(1.0, 0.25),
                                       species_type = "I")
                                ))
  expect_equal(ideal_island, expected_ideal_island)
})

test_that("create_ideal_island is correct for anagenetic and anagenetic
          recolonisation (stac 3)", {
  island_spec <- create_test_island_spec(island_scenario = 53)
  ideal_island <- create_ideal_island(
    total_time = 1,
    island_spec = island_spec)
  expected_ideal_island <- list(
    branching_times = c(1.0, 0.5),
    stac = 3,
    missing_species = 0,
    all_colonisations = list(list(event_times = c(1.0, 0.5),
                                  species_type = "A"),
                             list(event_times = c(1.0, 0.25),
                                  species_type = "A")))
  expect_equal(ideal_island, expected_ideal_island)
})

test_that("create_ideal_island is correct for cladogenetic and anagenetic
          recolonisation (stac 3)", {
  island_spec <- create_test_island_spec(island_scenario = 54)
  ideal_island <- create_ideal_island(
    total_time = 1,
    island_spec = island_spec)
  expected_ideal_island <- list(
    branching_times = c(1.0, 0.5, 0.4),
    stac = 3,
    missing_species = 0,
    all_colonisations = list(list(event_times = c(1.0, 0.5, 0.4),
                                  species_type = "C"),
                             list(event_times = c(1.0, 0.25),
                                  species_type = "A")))
  expect_equal(ideal_island, expected_ideal_island)
})
