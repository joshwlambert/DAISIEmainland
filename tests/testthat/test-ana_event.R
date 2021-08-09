test_that("ana_event produces correct output for a single species island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  island_spec <- create_test_island_spec(island_scenario = 1)
  max_spec_id <- 1

  updated_state <- ana_event(
    island_spec = island_spec,
    max_spec_id = max_spec_id)

  expect_true(is.list(updated_state))
  expected_island_spec <- data.frame(spec_id = 2,
                                     main_anc_id = 1,
                                     col_t = 0.2,
                                     spec_type = "A",
                                     branch_code = as.character(NA),
                                     branch_t = NaN,
                                     ana_origin = "immig_parent")
  expect_equal(updated_state$island_spec, expected_island_spec)
  expect_equal(updated_state$max_spec_id, 2)
})

test_that("ana_event produces correct output for a  multi-species island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection")
  island_spec <- create_test_island_spec(island_scenario = 36)
  max_spec_id <- 3

  updated_state <- ana_event(
    island_spec = island_spec,
    max_spec_id = max_spec_id)

  expect_true(is.list(updated_state))
  expected_island_spec <- data.frame(spec_id = c(1, 4),
                                     main_anc_id = c(1, 3),
                                     col_t = c(0.2, 0.65),
                                     spec_type = c("A", "A"),
                                     branch_code = c(as.character(NA),
                                                     as.character(NA)),
                                     branch_t = c(NaN, NaN),
                                     ana_origin = c("mainland_extinction",
                                                    "immig_parent"))
  expect_equal(updated_state$island_spec, expected_island_spec)
  expect_equal(updated_state$max_spec_id, 4)
})

test_that("ana_event fails with incorrect input", {

  island_spec <- create_test_island_spec(island_scenario = 1)

  expect_error(ana_event(
    island_spec = "nonsense",
    max_spec_id = 1)
  )

  expect_error(ana_event(
    island_spec = island_spec,
    max_spec_id = "nonsense")
  )
})
