test_that("ana_event produces correct output", {
  set.seed(1)
  island_spec <- data.frame(spec_id = 1,
                            main_anc_id = 1,
                            col_t = 0.5,
                            spec_type = "I",
                            branch_code = as.character(NA),
                            branch_t = NaN,
                            ana_origin = as.character(NA))
  max_spec_id <- 1

  updated_state <- ana_event(
    island_spec = island_spec,
    max_spec_id = max_spec_id)

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
