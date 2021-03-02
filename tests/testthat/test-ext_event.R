test_that("ext_event produces correct output for single species", {
  skip("WIP")
  set.seed(1)
  island_spec <- data.frame(spec_id = 1,
                            main_anc_id = 1,
                            col_t = 0.5,
                            spec_type = "I",
                            branch_code = NA,
                            branch_t = NA,
                            ana_origin = NA)
  island_spec <- ext_event(
    island_spec = island_spec)

  expect_true(is.data.frame(island_spec))
  expected_island_spec <- data.frame(spec_id = numeric(),
                                     main_anc_id = numeric(),
                                     col_t = numeric(),
                                     spec_type = character(),
                                     branch_code = character(),
                                     branch_t = numeric(),
                                     ana_origin = character())
  expect_equal(island_spec, expected_island_spec)
})

test_that("ext_event produces correct output for more than one species", {
  skip("WIP")
  set.seed(1)
  island_spec <- data.frame(spec_id = c(1, 2),
                            main_anc_id = c(1, 1),
                            col_t = c(0.5, 0.5),
                            spec_type = c("C", "C"),
                            branch_code = c("A", "B"),
                            branch_t = c(0.5, 0.6),
                            ana_origin = c(NA, NA))
  island_spec <- ext_event(
    island_spec = island_spec)

  expect_true(is.data.frame(island_spec))
  expected_island_spec <- data.frame(spec_id = 2,
                                     main_anc_id = 1,
                                     col_t = 0.5,
                                     spec_type = "A",
                                     branch_code = NA,
                                     branch_t = NA,
                                     ana_origin = "clado_extinct")
  expect_equal(island_spec, expected_island_spec)
})
