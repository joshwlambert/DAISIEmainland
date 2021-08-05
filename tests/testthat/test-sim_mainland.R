test_that("sim_mainland produces correct output", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )

  mainland <- sim_mainland(
    total_time = 1,
    m = 10,
    mainland_ex = 1)

  expect_true(is.list(mainland))
  expect_length(mainland, 10)
  expect_equal(ncol(mainland[[1]]), 7)
  expect_equal(mainland[[1]],
               data.frame(
                 spec_id = c(1, 15, 16, 21, 22, 27, 28),
                 main_anc_id = c(1, 1, 1, 1, 1, 1, 1),
                 spec_type = c("E", "E", "C", "E", "E", "C", "C"),
                 branch_code = c("A", "AA", "AB", "AAA", "AAB", "AABA", "AABB"),
                 branch_t = c(NaN, 0.292906805531114, 0.292906805531114,
                              0.518408027733462, 0.518408027733462,
                              0.734671684538218, 0.734671684538218),
                 spec_origin_t = c(0, 0.292906805531114, 0.292906805531114,
                                   0.518408027733462, 0.518408027733462,
                                   0.734671684538218, 0.734671684538218),
                 spec_ex_t = c(0.292906805531114, 0.518408027733462, 1,
                               0.541999222479509, 0.734671684538218, 1, 1)))
})

test_that("sim_mainland fails with incorrect input", {
  expect_error(sim_mainland(
    total_time = "nonsense",
    m = 100,
    mainland_ex = 1)
  )

  expect_error(sim_mainland(
    total_time = 1,
    m = "nonsense",
    mainland_ex = 1)
  )

  expect_error(sim_mainland(
    total_time = 1,
    m = 10,
    mainland_ex = "nonsense")
  )
})
