test_that("sim_mainland produces correct output", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )

  mainland <- sim_mainland(
    time = 1,
    m = 10,
    mainland_ex = 1)

  expect_true(is.list(mainland))
  expect_length(mainland, 10)
  expect_equal(ncol(mainland[[1]]), 7)
  expect_equal(mainland[[1]],
               data.frame(
                 spec_id = c(1, 15, 16, 23, 24),
                 main_anc_id = c(1, 1, 1, 1, 1),
                 spec_type = c("E", "E", "C", "C", "E"),
                 branch_code = c("A", "AA", "AB", "AAA", "AAB"),
                 branch_t = c(NA, 0.292906805531114, 0.292906805531114,
                              0.541999222479509, 0.541999222479509),
                 spec_origin_t = c(0, 0.292906805531114, 0.292906805531114,
                                   0.541999222479509, 0.541999222479509),
                 spec_ex_t = c(0.292906805531114, 0.541999222479509, 1, 1,
                               0.600847194624813)))
})
