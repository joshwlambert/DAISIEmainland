test_that("ext_event produces correct output for single species", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  island_tbl <- data.frame(
    spec_id = 1,
    main_anc_id = 1,
    col_t = 0.5,
    spec_type = "I",
    branch_code = as.character(NA),
    branch_t = NaN,
    ana_origin = as.character(NA)
  )
  island_tbl <- ext_event(
    island_tbl = island_tbl
  )

  expect_true(is.data.frame(island_tbl))
  expected_island_tbl <- data.frame(
    spec_id = numeric(),
    main_anc_id = numeric(),
    col_t = numeric(),
    spec_type = character(),
    branch_code = character(),
    branch_t = numeric(),
    ana_origin = character()
  )
  expect_equal(island_tbl, expected_island_tbl)
})

test_that("ext_event produces correct output for more than one species", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  island_tbl <- data.frame(
    spec_id = c(1, 2),
    main_anc_id = c(1, 1),
    col_t = c(0.5, 0.5),
    spec_type = c("C", "C"),
    branch_code = c("A", "B"),
    branch_t = c(0.5, 0.6),
    ana_origin = c(NA, NA)
  )
  island_tbl <- ext_event(
    island_tbl = island_tbl
  )

  expect_true(is.data.frame(island_tbl))
  expected_island_tbl <- data.frame(
    spec_id = 2,
    main_anc_id = 1,
    col_t = 0.5,
    spec_type = "A",
    branch_code = as.character(NA),
    branch_t = NaN,
    ana_origin = "clado_extinct"
  )
  row.names(island_tbl) <- NULL
  row.names(expected_island_tbl) <- NULL
  expect_equal(island_tbl, expected_island_tbl)
})

test_that("ext_event produces correct output for more than two species", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  island_tbl <- data.frame(
    spec_id = c(1, 2, 3, 4),
    main_anc_id = c(1, 1, 1, 1),
    col_t = c(0.5, 0.5, 0.5, 0.5),
    spec_type = c("C", "C", "C", "C"),
    branch_code = c("AAA", "B", "AB", "AAB"),
    branch_t = c(0.5, 0.6, 0.7, 0.8),
    ana_origin = c(NA, NA, NA, NA)
  )
  island_tbl <- ext_event(
    island_tbl = island_tbl
  )

  expect_true(is.data.frame(island_tbl))
  expected_island_tbl <- data.frame(
    spec_id = c(2, 3, 4),
    main_anc_id = c(1, 1, 1),
    col_t = c(0.5, 0.5, 0.5),
    spec_type = c("C", "C", "C"),
    branch_code = c("B", "AB", "AA"),
    branch_t = c(0.6, 0.7, 0.5),
    ana_origin = c(NA, NA, NA)
  )
  row.names(island_tbl) <- NULL
  row.names(expected_island_tbl) <- NULL
  expect_equal(island_tbl, expected_island_tbl)
})

test_that("ext_event fails with incorrect input", {
  island_tbl <- create_test_island_tbl(island_scenario = 1)

  expect_error(ext_event(
    island_tbl = "nonsense"
  ))
})
