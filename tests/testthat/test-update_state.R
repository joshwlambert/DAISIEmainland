test_that("update_state produces correct output for immigration", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  island_spec <- create_test_island_spec(island_scenario = 0)

  updated_state <- update_state(
    timeval = 0.5,
    total_time = 1.0,
    possible_event = 1,
    max_spec_id = 1,
    mainland_spec = 1,
    island_spec = island_spec)

  expect_true(is.list(updated_state))
  expected_island_spec <- data.frame(spec_id = 1,
                                     main_anc_id = 1,
                                     col_t = 0.5,
                                     spec_type = "I",
                                     branch_code = as.character(NA),
                                     branch_t = NaN,
                                     ana_origin = as.character(NA))
  expect_equal(updated_state$island_spec, expected_island_spec)
  expect_equal(updated_state$max_spec_id, 1)
})

test_that("update_state produces correct output for extinction", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  island_spec <- data.frame(spec_id = c(2, 3),
                            main_anc_id = c(1, 1),
                            col_t = c(0.5, 0.5),
                            spec_type = c("C", "C"),
                            branch_code = c("A", "B"),
                            branch_t = c(0.5, 0.6),
                            ana_origin = c(NA, NA))

  updated_state <- update_state(
    timeval = 0.7,
    total_time = 1.0,
    possible_event = 2,
    max_spec_id = 3,
    mainland_spec = 1,
    island_spec = island_spec)

  expect_true(is.list(updated_state))
  expected_island_spec <- data.frame(spec_id = 3,
                                     main_anc_id = 1,
                                     col_t = 0.5,
                                     spec_type = "A",
                                     branch_code = as.character(NA),
                                     branch_t = NaN,
                                     ana_origin = "clado_extinct")
  row.names(updated_state$island_spec) <- NULL
  row.names(expected_island_spec) <- NULL
  expect_equal(updated_state$island_spec, expected_island_spec)
  expect_equal(updated_state$max_spec_id, 3)
})

test_that("update_state produces correct output for anagenesis", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  island_spec <- data.frame(spec_id = 1,
                            main_anc_id = 1,
                            col_t = 0.5,
                            spec_type = "I",
                            branch_code = as.character(NA),
                            branch_t = NaN,
                            ana_origin = as.character(NA))

  updated_state <- update_state(
    timeval = 0.7,
    total_time = 1.0,
    possible_event = 3,
    max_spec_id = 1,
    mainland_spec = 1,
    island_spec = island_spec)

  expect_true(is.list(updated_state))
  expected_island_spec <- data.frame(spec_id = 2,
                                     main_anc_id = 1,
                                     col_t = 0.5,
                                     spec_type = "A",
                                     branch_code = as.character(NA),
                                     branch_t = NaN,
                                     ana_origin = "immig_parent")
  expect_equal(updated_state$island_spec, expected_island_spec)
  expect_equal(updated_state$max_spec_id, 2)
})

test_that("update_state produces correct output for cladogenesis", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  island_spec <- data.frame(spec_id = 1,
                            main_anc_id = 1,
                            col_t = 0.5,
                            spec_type = "I",
                            branch_code = as.character(NA),
                            branch_t = NaN,
                            ana_origin = as.character(NA))

  updated_state <- update_state(
    timeval = 0.7,
    total_time = 1.0,
    possible_event = 4,
    max_spec_id = 1,
    mainland_spec = 1,
    island_spec = island_spec)

  expect_true(is.list(updated_state))
  expected_island_spec <- data.frame(spec_id = c(2, 3),
                                     main_anc_id = c(1, 1),
                                     col_t = c(0.5, 0.5),
                                     spec_type = c("C", "C"),
                                     branch_code = c("A", "B"),
                                     branch_t = c(0.5, 0.7),
                                     ana_origin = c(as.character(NA),
                                                    as.character(NA)))
  expect_equal(updated_state$island_spec, expected_island_spec)
  expect_equal(updated_state$max_spec_id, 3)
})

test_that("update_state fails with incorrect input", {

  island_spec <- create_test_island_spec(island_scenario = 1)

  expect_error(update_state(
    timeval = "nonsense",
    total_time = 1,
    possible_event = 1,
    max_spec_id = 1,
    mainland_spec = 1,
    island_spec = island_spec)
  )

  expect_error(update_state(
    timeval = 0.5,
    total_time = "nonsense",
    possible_event = 1,
    max_spec_id = 1,
    mainland_spec = 1,
    island_spec = island_spec)
  )

  expect_error(update_state(
    timeval = 0.5,
    total_time = 1,
    possible_event = "nonsense",
    max_spec_id = 1,
    mainland_spec = 1,
    island_spec = island_spec)
  )

  expect_error(update_state(
    timeval = 0.5,
    total_time = 1,
    possible_event = 1,
    max_spec_id = "nonsense",
    mainland_spec = 1,
    island_spec = island_spec)
  )

  expect_error(update_state(
    timeval = 0.5,
    total_time = 1,
    possible_event = 1,
    max_spec_id = 1,
    mainland_spec = "nonsense",
    island_spec = island_spec)
  )

  expect_error(update_state(
    timeval = 0.5,
    total_time = 1,
    possible_event = 1,
    max_spec_id = 1,
    mainland_spec = 1,
    island_spec = "nonsense")
  )
})


