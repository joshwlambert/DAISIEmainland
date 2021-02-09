context("check_island_state")

test_that("check_island_state produces correct output with empty island", {
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
})

test_that("check_island_state produces correct output with non-empty island", {
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
  expect_equal(island_state,
               rbind(c("15", "15", "0.292906805531114", "A", NA, NA,
                       "mainland_extinction")))
})
