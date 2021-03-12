test_that("update_island_endemics produces correct output with empty island", {

  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)

  island_state <- update_island_endemics(
    timeval = 0.05858136,
    total_time = 1,
    island_spec = NULL,
    mainland_clade = mainland_clade)
  expect_null(island_state)
})

test_that("update_island_endemics produces correct output with non-empty
          island", {

  mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)

  island_state <- update_island_endemics(
    total_time = 1,
    timeval = 0.6,
    island_spec = data.frame(
      spec_id = 15,
      main_anc_id = 15,
      col_t = 0.292906805531114,
      spec_type = "I",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "mainland_extinction"),
    mainland_clade = mainland_clade)

  expect_equal(island_state,
               data.frame(spec_id = 15,
                          main_anc_id = 15,
                          col_t = 0.292906805531114,
                          spec_type = "A",
                          branch_code = as.character(NA),
                          branch_t = NaN,
                          ana_origin = "mainland_extinction"))
})
