test_that("immig_event produces correct output", {
  set.seed(1)
  timeval <- 0.1
  island_spec <- data.frame(spec_id = numeric(),
                            main_anc_id = numeric(),
                            col_t = numeric(),
                            spec_type = character(),
                            branch_code = character(),
                            branch_t = numeric(),
                            ana_origin = character())
  mainland_spec <- 1
  island_spec <- immig_event(
    timeval = timeval,
    island_spec = island_spec,
    mainland_spec = mainland_spec)
  expect_true(is.data.frame(island_spec))
  expected_island_spec <- data.frame(spec_id = 1,
                                     main_anc_id = 1,
                                     col_t = 0.1,
                                     spec_type = "I",
                                     branch_code = NA,
                                     branch_t = NA,
                                     ana_origin = NA)
  expect_equal(island_spec, expected_island_spec)
})
