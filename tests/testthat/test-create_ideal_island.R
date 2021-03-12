test_that("create_ideal_island is correct for singleton endemic (stac 2)", {
  island_spec <- data.frame(spec_id = 2,
                            main_anc_id = 1,
                            col_t_bp = 0.5,
                            spec_type = "A",
                            branch_code = as.character(NA),
                            branch_t_bp = NaN,
                            ana_origin = "immig_parent")
  ideal_island <- create_ideal_island(
    total_time = 1,
    island_spec = island_spec)
  expected_ideal_island <- list(branching_times = c(1.0, 0.5),
                                stac = 2,
                                missing_species = 0)
  expect_equal(ideal_island, expected_ideal_island)
})

test_that("create_ideal_island is correct for endemic clade (stac 2)", {
  island_spec <- data.frame(spec_id = c(2, 3),
                            main_anc_id = c(1, 1),
                            col_t_bp = c(0.5, 0.5),
                            spec_type = c("C", "C"),
                            branch_code = c("A", "B"),
                            branch_t_bp = c(0.5, 0.3),
                            ana_origin = c(NA, NA))
  ideal_island <- create_ideal_island(
    total_time = 1,
    island_spec = island_spec)
  expected_ideal_island <- list(branching_times = c(1.0, 0.5, 0.3),
                                stac = 2,
                                missing_species = 0)
  expect_equal(ideal_island, expected_ideal_island)
})

test_that("create_ideal_island is correct for one recolonisation (stac 3)", {
  island_spec <- data.frame(spec_id = c(1, 2),
                            main_anc_id = c(1, 2),
                            col_t_bp = c(0.5, 0.3),
                            spec_type = c("A", "I"),
                            branch_code = as.character(NA),
                            branch_t_bp = NaN,
                            ana_origin = "immig_parent")
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

test_that("create_ideal_island is correct for singletone nonendemic (stac 4)", {
  island_spec <- data.frame(spec_id = 1,
                            main_anc_id = 1,
                            col_t_bp = 0.5,
                            spec_type = "I",
                            branch_code = as.character(NA),
                            branch_t_bp = NaN,
                            ana_origin = as.character(NA))
  ideal_island <- create_ideal_island(
    total_time = 1,
    island_spec = island_spec)

  expected_ideal_island <- list(branching_times = c(1.0, 0.5),
                                stac = 4,
                                missing_species = 0)
  expect_equal(ideal_island, expected_ideal_island)
})





test_that("create_ideal_island is correct for >=2 cladogenetic with same
          ancestor", {
})


test_that("create_ideal_island works with >=2 anagenetic with same ancestor", {
})

test_that("create_ideal_island works with >=2 nonendemic with same ancestor", {
})

test_that("create_ideal_island stac and brts works for single colonist", {
})

test_that("create_ideal_island stac and brts works for 1 nonendemic colonist", {
})

test_that("create_ideal_island stac and brts works for 2 endemic colonists,
          1 nonendemic", {
})

test_that("create_ideal_island stac and brts works for 3 endemic colonists", {
})


test_that("create_ideal_island stac and brts works for 2 endemic clades", {
})

test_that("create_ideal_island stac and brts works for 2 endemic clades,
          1 nonendemic", {
})

test_that("create_ideal_island stac and brts works for 2 endemic clades,
          1 endemic singleton", {
})


test_that("create_ideal_island stac and brts works for 1 anagenetic clade from
          extinction of cladogenetic and 1 nonendemic recolonist ", {
})
