test_that("sample_mainland runs silent and produces correct output complete
          sampling", {
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 3)

  expect_silent(sampled_mainland <- sample_mainland(
    total_time = 1,
    mainland_clade = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered"
  ))
  expect_equal(mainland_clade, sampled_mainland)
})

test_that("sample_mainland runs silent and produces correct output for
          unsampled species", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )

  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  expect_silent(sampled_mainland <- sample_mainland(
    total_time = 1,
    mainland_clade = mainland_clade,
    mainland_sample_prob = 0.5,
    mainland_sample_type = "unsampled"
  ))
  new_mainland <- data.frame(
    spec_id = 1,
    main_anc_id = 1,
    spec_type = "US",
    branch_code = "A",
    branch_t = NaN,
    spec_origin_t = 0,
    spec_ex_t = 0.99999
  )

  expect_equal(new_mainland, sampled_mainland)
})

test_that("sample_mainland runs silent and produces correct output for
          undiscovered species", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )

  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  expect_silent(sampled_mainland <- sample_mainland(
    total_time = 1,
    mainland_clade = mainland_clade,
    mainland_sample_prob = 0.5,
    mainland_sample_type = "undiscovered"
  ))
  new_mainland <- data.frame(
    spec_id = 1,
    main_anc_id = 1,
    spec_type = "UD",
    branch_code = "A",
    branch_t = NaN,
    spec_origin_t = 0,
    spec_ex_t = 0.99999
  )

  expect_equal(new_mainland, sampled_mainland)
})
