context("sim_mainland")

test_that("sim_mainland produces correct output", {
  set.seed(1)
  mainland <- sim_mainland(
    time = 1,
    m = 10,
    mainland_ext = 1)
  expect_true(is.list(mainland))
  expect_length(mainland, 10)
  expect_length(mainland[[1]][1, ], 9)
  expect_equal(mainland[[1]],
               rbind(c("1", "1", "0", "E", "A", NA, NA, "0",
                       "0.292906805531114"),
                     c("15", "1", "0", "E", "AA", "0.292906805531114", NA,
                       "0.292906805531114", "0.541999222479509"),
                     c("16", "1", "0", "C", "AB", "0.292906805531114", NA,
                       "0.292906805531114", "1"),
                     c("23", "1", "0", "C", "AAA", "0.541999222479509", NA,
                       "0.541999222479509", "1"),
                     c("24", "1", "0", "E", "AAB", "0.541999222479509", NA,
                       "0.541999222479509", "0.600847194624813")))
})
