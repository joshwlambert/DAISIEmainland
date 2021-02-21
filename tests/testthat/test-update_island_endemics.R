test_that("update_island_endemics produces correct output with empty island", {

  mainland <- create_test_mainland(mainland_scenario = 1)

  island_state <- update_island_endemics(
    timeval = 0.05858136,
    totaltime = 1,
    island_spec = NULL,
    mainland = mainland[[1]])
  expect_null(island_state)
})

test_that("update_island_endemics produces correct output with non-empty island", {

  mainland <- create_test_mainland(mainland_scenario = 2)

  island_state <- update_island_endemics(
    totaltime = 1,
    timeval = 0.6,
    island_spec = data.frame(
      spec_id = 15,
      main_anc_id = 15,
      col_t = 0.292906805531114,
      spec_type = "I",
      branch_code = NA,
      branch_t = NA,
      ana_origin = "mainland_extinction"),
    mainland = mainland[[1]])

  expect_equal(island_state,
               data.frame(spec_id = c(15),
                          main_anc_id = c(15),
                          col_t = c(0.292906805531114),
                          spec_type = c("A"),
                          branch_code = c(NA),
                          branch_t = c(NA),
                          ana_origin = c("mainland_extinction")))
})
