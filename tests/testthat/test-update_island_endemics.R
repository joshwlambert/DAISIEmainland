test_that("update_island_endemics produces correct output with empty island", {

  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  island_tbl <- create_test_island_tbl(island_scenario = 0)

  island_tbl <- update_island_endemics(
    timeval = 0.05858136,
    total_time = 1,
    island_tbl = island_tbl,
    mainland_clade = mainland_clade)
  expect_true(is.data.frame(island_tbl))
  expect_true(nrow(island_tbl) == 0)
  expect_true(ncol(island_tbl) == 7)
})

test_that("update_island_endemics produces correct output with no non-endemics
          on the island", {

  mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)

  island_tbl <- update_island_endemics(
    total_time = 1,
    timeval = 0.6,
    island_tbl = data.frame(
      spec_id = 4,
      main_anc_id = 1,
      col_t = 0.2,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "immig_parent"),
    mainland_clade = mainland_clade)

  expect_equal(island_tbl,
               data.frame(spec_id = 4,
                          main_anc_id = 1,
                          col_t = 0.2,
                          spec_type = "A",
                          branch_code = as.character(NA),
                          branch_t = NaN,
                          ana_origin = "immig_parent"))
})

test_that("update_island_endemics produces correct output with non-endemics on
          the island that change to endemic through mainland extinction", {

  mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)

  island_tbl <- update_island_endemics(
    total_time = 1,
    timeval = 0.6,
    island_tbl = data.frame(
      spec_id = 1,
      main_anc_id = 1,
      col_t = 0.2,
      spec_type = "I",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = as.character(NA)),
    mainland_clade = mainland_clade)

  expect_equal(island_tbl,
               data.frame(spec_id = 1,
                          main_anc_id = 1,
                          col_t = 0.2,
                          spec_type = "A",
                          branch_code = as.character(NA),
                          branch_t = NaN,
                          ana_origin = "mainland_extinction"))
})

test_that("update_island_endemics produces correct output with non-endemics on
          the island that do not change to endemic through mainland
          extinction", {

  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)

  island_tbl <- update_island_endemics(
    total_time = 1,
    timeval = 0.6,
    island_tbl = data.frame(
      spec_id = 1,
      main_anc_id = 1,
      col_t = 0.2,
      spec_type = "I",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = as.character(NA)),
    mainland_clade = mainland_clade)

  expect_equal(island_tbl,
               data.frame(spec_id = 1,
                          main_anc_id = 1,
                          col_t = 0.2,
                          spec_type = "I",
                          branch_code = as.character(NA),
                          branch_t = NaN,
                          ana_origin = as.character(NA)))
})

test_that("update_island_endemics fails with incorrect input", {

  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  island_tbl <- create_test_island_tbl(island_scenario = 1)

  expect_error(update_island_endemics(
    timeval = "nonsense",
    total_time = 1,
    island_tbl = island_tbl,
    mainland_clade = mainland_clade)
  )

  expect_error(update_island_endemics(
    timeval = 0.5,
    total_time = "nonsense",
    island_tbl = island_tbl,
    mainland_clade = mainland_clade)
  )

  expect_error(update_island_endemics(
    timeval = 0.5,
    total_time = 1,
    island_tbl = "nonsense",
    mainland_clade = mainland_clade)
  )

  expect_error(update_island_endemics(
    timeval = 0.5,
    total_time = 1,
    island_tbl = island_tbl,
    mainland_clade = "nonsense")
  )
})
