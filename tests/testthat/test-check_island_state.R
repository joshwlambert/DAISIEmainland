context("check_island_state")

test_that("check_island_state produces correct output with empty island", {
  # RJCB: I would love a 'create_test_mainland_1' function here!
  mainland <- rbind(
    c("1", "1", "0", "E", "A", NA, NA, "0", "0.292906805531114"),
    c("15", "1", "0", "E", "AA", "0.292906805531114", NA, "0.292906805531114",
      "0.541999222479509"),
    c("16", "1", "0", "C", "AB", "0.292906805531114", NA, "0.292906805531114",
      "1"))

  island_state <- check_island_state(
    timeval = 0.05858136,
    totaltime = 1,
    island_spec = NULL,
    mainland = mainland)
  expect_null(island_state)
})

test_that("check_island_state produces correct output with non-empty island", {
  # RJCB: I would love a 'create_test_mainland_2' function here!
  mainland <- rbind(
    c("1", "1", "0", "E", "A", NA, NA, "0", "0.292906805531114"),
    c("15", "1", "0", "E", "AA", "0.292906805531114", NA, "0.292906805531114",
      "0.541999222479509"),
    c("16", "1", "0", "C", "AB", "0.292906805531114", NA, "0.292906805531114",
      "1"))

  island_state <- check_island_state(
    totaltime = 1,
    timeval = 0.6,
    island_spec = rbind(c("15", "15", "0.292906805531114", "I", NA, NA, NA)),
    mainland = mainland)

  # I enjoy this test. I wish I would understand what 'check_island_state'
  # does, so I could agree with this results by reading the input
  expect_equal(island_state,
               rbind(c("15", "15", "0.292906805531114", "A", NA, NA,
                       "mainland_extinction")))
})
