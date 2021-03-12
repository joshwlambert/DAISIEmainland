test_that("clado_event produces correct output on non-cladogenesis species", {
  set.seed(1)
  timeval <- 0.6
  island_spec <- data.frame(spec_id = 1,
                            main_anc_id = 1,
                            col_t = 0.5,
                            spec_type = "I",
                            branch_code = as.character(NA),
                            branch_t = NaN,
                            ana_origin = as.character(NA))
  max_spec_id <- 1
  updated_state <- clado_event(
    timeval = timeval,
    island_spec = island_spec,
    max_spec_id = max_spec_id)

  expect_true(is.list(updated_state))
  expected_island_spec <- data.frame(spec_id = c(2, 3),
                                     main_anc_id = c(1, 1),
                                     col_t = c(0.5, 0.5),
                                     spec_type = c("C", "C"),
                                     branch_code = c("A", "B"),
                                     branch_t = c(0.5, 0.6),
                                     ana_origin = c(as.character(NA),
                                                    as.character(NA)))
  expect_equal(updated_state$island_spec, expected_island_spec)
  expect_equal(updated_state$max_spec_id, 3)
})

test_that("clado_event produces correct output on cladogenesis species", {
  set.seed(1)
  timeval <- 0.9
  island_spec <- data.frame(spec_id = c(2, 3),
                            main_anc_id = c(1, 1),
                            col_t = c(0.5, 0.5),
                            spec_type = c("C", "C"),
                            branch_code = c("A", "B"),
                            branch_t = c(0.5, 0.7),
                            ana_origin = c(NA, NA))
  max_spec_id <- 3
  updated_state <- clado_event(
    timeval = timeval,
    island_spec = island_spec,
    max_spec_id = max_spec_id)

  expect_true(is.list(updated_state))
  expected_island_spec <- data.frame(spec_id = c(4, 3, 5),
                                     main_anc_id = c(1, 1, 1),
                                     col_t = c(0.5, 0.5, 0.5),
                                     spec_type = c("C", "C", "C"),
                                     branch_code = c("AA", "B", "AB"),
                                     branch_t = c(0.5, 0.7, 0.9),
                                     ana_origin = c(as.character(NA),
                                                    as.character(NA),
                                                    as.character(NA)))
  expect_equal(updated_state$island_spec, expected_island_spec)
  expect_equal(updated_state$max_spec_id, 5)
})
