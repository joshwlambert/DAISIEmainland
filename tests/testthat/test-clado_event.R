test_that("multiplication works", {
  set.seed(1)
  timeval <- 0.6
  island_spec <- data.frame(spec_id = 1,
                            main_anc_id = 1,
                            col_t = 0.5,
                            spec_type = "I",
                            branch_code = NA,
                            branch_t = NA,
                            ana_origin = NA)
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
                                     ana_origin = c(NA, NA))
  expect_equal(updated_state$island_spec, expected_island_spec)
  expect_equal(updated_state$max_spec_id, 3)
})
