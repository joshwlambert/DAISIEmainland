test_that("sim_island is silent and produces correct empty island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  expect_silent(
    island_tbl <- sim_island(
      total_time = 1,
      island_pars = c(1, 1, 10, 1, 1),
      mainland_clade = mainland_clade,
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
    )
  )
  expect_true(is.data.frame(island_tbl))
  expect_equal(nrow(island_tbl), 0)
  expect_equal(ncol(island_tbl), 7)
  expect_identical(
    names(island_tbl),
    c(
      "spec_id", "main_anc_id", "col_t", "spec_type",
      "branch_code", "branch_t", "ana_origin"
    )
  )
})

test_that("sim_island is silent and produces correct non-empty island", {
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
  expect_silent(
    island_tbl <- sim_island(
      total_time = 1,
      island_pars = c(1, 1, 10, 1, 1),
      mainland = mainland_clade,
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
    )
  )
  expect_silent(check_island_tbl(island_tbl))
  expect_equal(nrow(island_tbl), 4)
  expect_equal(island_tbl$spec_id, c(5, 2, 6, 7))
  expect_equal(island_tbl$main_anc_id, c(3, 2, 3, 3))
  expect_equal(
    island_tbl$col_t,
    c(
      0.702374035958, 0.740646675526,
      0.835363711978, 0.835363711978
    )
  )
  expect_identical(island_tbl$spec_type, c("A", "I", "C", "C"))
  expect_identical(
    island_tbl$branch_code,
    c(as.character(NA), as.character(NA), "A", "B")
  )
  expect_equal(
    island_tbl$branch_t,
    c(NaN, NaN, 0.835363711978, 0.873457537506)
  )
  expect_identical(
    island_tbl$ana_origin,
    c(
      "clado_extinct", as.character(NA),
      as.character(NA), as.character(NA)
    )
  )
})

test_that("sim_island fails with incorrect input", {
  expect_error(sim_island(
    total_time = "nonsense",
    island_pars = c(1, 1, 10, 1, 1),
    mainland_clade = create_test_mainland_clade(mainland_scenario = 1),
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  ))

  expect_error(sim_island(
    total_time = 1,
    island_pars = "nonsense",
    mainland_clade = create_test_mainland_clade(mainland_scenario = 1),
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  ))

  expect_error(sim_island(
    total_time = 1,
    island_pars = c("nonsense", 1, 10, 1, 1),
    mainland_clade = create_test_mainland_clade(mainland_scenario = 1),
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  ))

  expect_error(sim_island(
    total_time = 1,
    island_pars = c(1, "nonsense", 10, 1, 1),
    mainland_clade = create_test_mainland_clade(mainland_scenario = 1),
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  ))

  expect_error(sim_island(
    total_time = 1,
    island_pars = c(1, 1, "nonsense", 1, 1),
    mainland_clade = create_test_mainland_clade(mainland_scenario = 1),
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  ))

  expect_error(sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, "nonsense", 1),
    mainland_clade = create_test_mainland_clade(mainland_scenario = 1),
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  ))

  expect_error(sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, "nonsense"),
    mainland_clade = create_test_mainland_clade(mainland_scenario = 1),
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  ))

  expect_error(sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_clade = "nonsense",
    mainland_sample_prob = 1,
    mainland_sample_type = "complete"
  ))

  expect_error(sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_clade = create_test_mainland_clade(mainland_scenario = 1),
    mainland_sample_prob = "nonsense",
    mainland_sample_type = "complete"
  ))

  expect_error(sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, 1),
    mainland_clade = create_test_mainland_clade(mainland_scenario = 1),
    mainland_sample_prob = 1,
    mainland_sample_type = "nonsense"
  ))
})
