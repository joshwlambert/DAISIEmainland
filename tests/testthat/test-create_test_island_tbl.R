test_that("create_test_island_tbl produces correct output for scenario 0", {
  island_tbl <- create_test_island_tbl(island_scenario = 0)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 0)
})

test_that("create_test_island_tbl produces correct output for scenario 1", {
  island_tbl <- create_test_island_tbl(island_scenario = 1)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("scenario 0 -> colonization -> scenario 1", {

  # Empty island
  island_tbl_before <- create_test_island_tbl(island_scenario = 0)
  island_tbl_after <- update_state(
    timeval = 0.16,
    total_time = 1.0,
    possible_event = str_to_event("immigration"),
    max_spec_id = 1,
    mainland_spec = 1,
    island_tbl = island_tbl_before
  )
  expect_identical(
    island_tbl_after$island_tbl,
    create_test_island_tbl(island_scenario = 1)
  )
})

test_that("create_test_island_tbl produces correct output for scenario 2", {
  island_tbl <- create_test_island_tbl(island_scenario = 2)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 3", {
  island_tbl <- create_test_island_tbl(island_scenario = 3)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 4", {
  island_tbl <- create_test_island_tbl(island_scenario = 4)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 5", {
  island_tbl <- create_test_island_tbl(island_scenario = 5)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 6", {
  island_tbl <- create_test_island_tbl(island_scenario = 6)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 7", {
  island_tbl <- create_test_island_tbl(island_scenario = 7)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 8", {
  island_tbl <- create_test_island_tbl(island_scenario = 8)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 9", {
  island_tbl <- create_test_island_tbl(island_scenario = 9)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 10", {
  island_tbl <- create_test_island_tbl(island_scenario = 10)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 11", {
  island_tbl <- create_test_island_tbl(island_scenario = 11)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 12", {
  island_tbl <- create_test_island_tbl(island_scenario = 12)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 13", {
  island_tbl <- create_test_island_tbl(island_scenario = 13)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 14", {
  island_tbl <- create_test_island_tbl(island_scenario = 14)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 15", {
  island_tbl <- create_test_island_tbl(island_scenario = 15)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 16", {
  island_tbl <- create_test_island_tbl(island_scenario = 16)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 17", {
  island_tbl <- create_test_island_tbl(island_scenario = 17)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 18", {
  island_tbl <- create_test_island_tbl(island_scenario = 18)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 19", {
  island_tbl <- create_test_island_tbl(island_scenario = 19)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 20", {
  island_tbl <- create_test_island_tbl(island_scenario = 20)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 21", {
  island_tbl <- create_test_island_tbl(island_scenario = 21)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 22", {
  island_tbl <- create_test_island_tbl(island_scenario = 22)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 23", {
  island_tbl <- create_test_island_tbl(island_scenario = 23)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 24", {
  island_tbl <- create_test_island_tbl(island_scenario = 24)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 25", {
  island_tbl <- create_test_island_tbl(island_scenario = 25)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 26", {
  island_tbl <- create_test_island_tbl(island_scenario = 26)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 27", {
  island_tbl <- create_test_island_tbl(island_scenario = 27)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 28", {
  island_tbl <- create_test_island_tbl(island_scenario = 28)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 3)
})

test_that("create_test_island_tbl produces correct output for scenario 29", {
  island_tbl <- create_test_island_tbl(island_scenario = 29)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 3)
})

test_that("create_test_island_tbl produces correct output for scenario 30", {
  island_tbl <- create_test_island_tbl(island_scenario = 30)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 3)
})

test_that("create_test_island_tbl produces correct output for scenario 31", {
  island_tbl <- create_test_island_tbl(island_scenario = 31)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 3)
})

test_that("create_test_island_tbl produces correct output for scenario 32", {
  island_tbl <- create_test_island_tbl(island_scenario = 32)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 3)
})

test_that("create_test_island_tbl produces correct output for scenario 33", {
  island_tbl <- create_test_island_tbl(island_scenario = 33)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 3)
})

test_that("create_test_island_tbl produces correct output for scenario 34", {
  island_tbl <- create_test_island_tbl(island_scenario = 34)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 3)
})

test_that("create_test_island_tbl produces correct output for scenario 35", {
  island_tbl <- create_test_island_tbl(island_scenario = 35)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 3)
})

test_that("create_test_island_tbl produces correct output for scenario 36", {
  island_tbl <- create_test_island_tbl(island_scenario = 36)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 37", {
  island_tbl <- create_test_island_tbl(island_scenario = 37)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 38", {
  island_tbl <- create_test_island_tbl(island_scenario = 38)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 39", {
  island_tbl <- create_test_island_tbl(island_scenario = 39)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 40", {
  island_tbl <- create_test_island_tbl(island_scenario = 40)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 41", {
  island_tbl <- create_test_island_tbl(island_scenario = 41)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 42", {
  island_tbl <- create_test_island_tbl(island_scenario = 42)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 43", {
  island_tbl <- create_test_island_tbl(island_scenario = 43)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 44", {
  island_tbl <- create_test_island_tbl(island_scenario = 44)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 45", {
  island_tbl <- create_test_island_tbl(island_scenario = 45)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 3)
})

test_that("create_test_island_tbl produces correct output for scenario 46", {
  island_tbl <- create_test_island_tbl(island_scenario = 46)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 47", {
  island_tbl <- create_test_island_tbl(island_scenario = 47)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 48", {
  island_tbl <- create_test_island_tbl(island_scenario = 48)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 49", {
  island_tbl <- create_test_island_tbl(island_scenario = 49)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 50", {
  island_tbl <- create_test_island_tbl(island_scenario = 50)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 51", {
  island_tbl <- create_test_island_tbl(island_scenario = 51)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 52", {
  island_tbl <- create_test_island_tbl(island_scenario = 52)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 3)
})

test_that("create_test_island_tbl produces correct output for scenario 53", {
  island_tbl <- create_test_island_tbl(island_scenario = 53)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 54", {
  island_tbl <- create_test_island_tbl(island_scenario = 54)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 3)
})

test_that("create_test_island_tbl produces correct output for scenario 55", {
  island_tbl <- create_test_island_tbl(island_scenario = 55)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 56", {
  island_tbl <- create_test_island_tbl(island_scenario = 56)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 1)
})

test_that("create_test_island_tbl produces correct output for scenario 57", {
  island_tbl <- create_test_island_tbl(island_scenario = 57)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl produces correct output for scenario 58", {
  island_tbl <- create_test_island_tbl(island_scenario = 58)
  expect_true(is.data.frame(island_tbl))
  expect_true(ncol(island_tbl) == 7)
  expect_true(nrow(island_tbl) == 2)
})

test_that("create_test_island_tbl fails correctly", {
  expect_error(create_test_island_tbl(island_scenario = -1))
  expect_error(create_test_island_tbl(island_scenario = 100))
})
