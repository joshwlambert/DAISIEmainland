# For reference on the appendix scenarios see:
# inst/extdata/DAISIEmainland_appendix_a.pdf

test_that("appendix scenario 0 (empty island)", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 0),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 1),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, 1)
   expect_identical(island$ideal_island[[1]]$stac, 0)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A1", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 1),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 1),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$ideal_island[[1]]$stac, 4)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A2", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 2),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 2),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A3", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 3),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 2),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[1]]$stac, 4)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A4", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 4),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 3),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times, c(1.00, 0.99999))
   expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   expect_identical(island$empirical_island[[1]]$stac, 5)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scnario A5", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 5),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 4),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A6", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 6),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 5),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times, c(1.00, 0.99999))
   expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   expect_identical(island$empirical_island[[1]]$stac, 5)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A7", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 7),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 4),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[1]]$stac, 4)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A8", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 8),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 6),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times, c(1.00, 0.5))
   expect_identical(island$empirical_island[[1]]$stac, 2)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A9", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 9),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 5),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times, c(1.00, 0.99999))
   expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   expect_identical(island$empirical_island[[1]]$stac, 5)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A10", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 10),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 1),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A11", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 11),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 2),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A12", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 12),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 2),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A13", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 13),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 3),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times, c(1.00, 0.99999))
   expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   expect_identical(island$empirical_island[[1]]$stac, 5)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A14", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 14),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 7),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A15", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 15),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 8),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times, c(1.00, 0.99999))
   expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   expect_identical(island$empirical_island[[1]]$stac, 5)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A16", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 16),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 4),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A17", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 17),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 6),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$ideal_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times, c(1.00, 0.5))
   expect_identical(island$empirical_island[[1]]$stac, 2)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A18", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 18),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 5),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times, c(1.00, 0.99999))
   expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   expect_identical(island$empirical_island[[1]]$stac, 5)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A19", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 19),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 1),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84, 0.50))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A20", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 20),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 2),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84, 0.42))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A21", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 21),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 3),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84, 0.5))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times,
                c(1.00, 0.99999, 0.5))
   expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   expect_identical(island$empirical_island[[1]]$stac, 6)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})


test_that("appendix scenario A22", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 22),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 2),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.33, 0.17))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A23", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 23),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 2),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84, 0.42))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A24", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 24),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 9),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84, 0.42))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times,
                c(1.00, 0.99999, 0.42))
   expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   expect_identical(island$empirical_island[[1]]$stac, 6)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A25", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 25),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 10),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.33, 0.17))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A26", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 26),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 6),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.33, 0.17))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times,
                c(1.00, 0.50, 0.17))
   expect_identical(island$empirical_island[[1]]$stac, 2)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A27", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 27),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 5),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.33, 0.09))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times,
                c(1.00, 0.99999, 0.09))
   expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   expect_identical(island$empirical_island[[1]]$stac, 6)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A28", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 28),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 1),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84, 0.67))
   expect_identical(island$ideal_island[[1]]$stac, 3)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A29", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 29),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 3),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84, 0.67))
   expect_identical(island$ideal_island[[1]]$stac, 3)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times,
                c(1.00, 0.99999, 0.84, 0.67))
   expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   expect_identical(island$empirical_island[[1]]$stac, 6)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A30", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 30),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 11),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84, 0.67))
   expect_identical(island$ideal_island[[1]]$stac, 3)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A31", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 31),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 12),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84, 0.67))
   expect_identical(island$ideal_island[[1]]$stac, 3)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A32", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 32),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 13),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84, 0.67))
   expect_identical(island$ideal_island[[1]]$stac, 3)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times,
                c(1.00, 0.99999, 0.84, 0.67))
   expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   expect_identical(island$empirical_island[[1]]$stac, 6)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A33", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 33),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 14),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.67, 0.5))
   expect_identical(island$ideal_island[[1]]$stac, 3)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A34", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 34),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 15),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.67, 0.5))
   expect_identical(island$ideal_island[[1]]$stac, 3)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times,
                c(1.00, 0.84, 0.67, 0.5))
   expect_identical(island$empirical_island[[1]]$stac, 2)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A35", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 35),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 16),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 1)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.67, 0.5))
   expect_identical(island$ideal_island[[1]]$stac, 3)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times,
                c(1.00, 0.99999, 0.67, 0.5))
   expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   expect_identical(island$empirical_island[[1]]$stac, 6)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A36", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 36),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 2),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 2)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_equal(island$ideal_island[[2]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[2]]$stac, 4)
   expect_identical(island$ideal_island[[2]]$missing_species, 0)
})

test_that("appendix scenario A37", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 37),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 4),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 2)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_equal(island$ideal_island[[2]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[2]]$stac, 4)
   expect_identical(island$ideal_island[[2]]$missing_species, 0)
})

test_that("appendix scenario A38", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 38),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 6),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 2)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_equal(island$ideal_island[[2]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[2]]$stac, 2)
   expect_identical(island$ideal_island[[2]]$missing_species, 0)
   expect_length(island$empirical_island, 2)
   expect_equal(island$empirical_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$empirical_island[[1]]$stac, 2)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
   expect_equal(island$empirical_island[[2]]$branching_times, c(1.00, 0.5))
   expect_identical(island$empirical_island[[2]]$stac, 2)
   expect_identical(island$empirical_island[[2]]$missing_species, 0)
})

test_that("appendix scenario A39", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 39),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 5),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 2)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.84))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_equal(island$ideal_island[[2]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[2]]$stac, 2)
   expect_identical(island$ideal_island[[2]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times,
                c(1.00, 0.99999, 0.84))
   expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   expect_identical(island$empirical_island[[1]]$stac, 6)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("appendix scenario A40", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 40),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 17),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 2)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.58))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_equal(island$ideal_island[[2]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[2]]$stac, 4)
   expect_identical(island$ideal_island[[2]]$missing_species, 0)
})

test_that("appendix scenario A41", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 41),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 18),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_identical(island$ideal_island, island$empirical_island)
   expect_length(island$ideal_island, 2)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.58))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_equal(island$ideal_island[[2]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[2]]$stac, 4)
   expect_identical(island$ideal_island[[2]]$missing_species, 0)
})

test_that("appendix scenario A42", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 42),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 19),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 2)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.58))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_equal(island$ideal_island[[2]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[2]]$stac, 2)
   expect_identical(island$ideal_island[[2]]$missing_species, 0)
   expect_length(island$empirical_island, 2)
   expect_equal(island$empirical_island[[1]]$branching_times, c(1.00, 0.58))
   expect_identical(island$empirical_island[[1]]$stac, 2)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
   expect_equal(island$empirical_island[[2]]$branching_times, c(1.00, 0.5))
   expect_identical(island$empirical_island[[2]]$stac, 2)
   expect_identical(island$empirical_island[[2]]$missing_species, 0)
})

test_that("appendix scenario A43", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   island <- create_daisie_data(
      total_time = 1,
      island_tbl = create_test_island_tbl(island_scenario = 43),
      mainland_clade = create_test_mainland_clade(mainland_scenario = 20),
      mainland_sample_prob = 1,
      mainland_sample_type = "complete"
   )
   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_length(island$ideal_island, 2)
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.00, 0.58))
   expect_identical(island$ideal_island[[1]]$stac, 2)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_equal(island$ideal_island[[2]]$branching_times, c(1.00, 0.33))
   expect_identical(island$ideal_island[[2]]$stac, 2)
   expect_identical(island$ideal_island[[2]]$missing_species, 0)
   expect_length(island$empirical_island, 1)
   expect_equal(island$empirical_island[[1]]$branching_times,
                c(1.00, 0.67, 0.58))
   expect_identical(island$empirical_island[[1]]$stac, 2)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("create_daisie_data fails with incorrect input", {
   mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
   island_tbl <- create_test_island_tbl(island_scenario = 0)
   expect_error(create_daisie_data(
      total_time = "nonsense",
      island_tbl = island_tbl,
      mainland_clade = mainland_clade,
      mainland_sample_prob = 1)
   )
   expect_error(create_daisie_data(
      total_time = 1,
      island_tbl = "nonsense",
      mainland_clade = mainland_clade,
      mainland_sample_prob = 1)
   )
   expect_error(create_daisie_data(
      total_time = 1,
      island_tbl = island_tbl,
      mainland_clade = "nonsense",
      mainland_sample_prob = 1)
   )
   expect_error(create_daisie_data(
      total_time = 1,
      island_tbl = island_tbl,
      mainland_clade = mainland_clade,
      mainland_sample_prob = "nonsense")
   )
})
