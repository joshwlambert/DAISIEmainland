test_that("create_island produces the same ideal and empirical empty islands", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1
   mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
   island_spec <- create_test_island_spec(island_scenario = 0)
   mainland_sample_prob <- 1
   mainland_sample_type <- "undiscovered"

   island <- create_island(
     total_time = total_time,
     island_spec = island_spec,
     mainland_clade = mainland_clade,
     mainland_sample_prob = mainland_sample_prob,
     mainland_sample_type = mainland_sample_type
   )

   expect_identical(island$ideal_island, island$empirical_island)
   expect_identical(island$ideal_island[[1]]$branching_times, 1)
   expect_identical(island$ideal_island[[1]]$stac, 0)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("create_island produces the same ideal and empirical non-empty
          islands", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1
   mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
   island_spec <- create_test_island_spec(island_scenario = 1)
   mainland_sample_prob <- 1
   mainland_sample_type <- "undiscovered"

   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob,
      mainland_sample_type = mainland_sample_type
   )

   expect_identical(island$ideal_island, island$empirical_island)
   expect_identical(island$ideal_island[[1]]$branching_times, c(1.0, 0.8))
   expect_identical(island$ideal_island[[1]]$stac, 4)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
})


test_that("create_island produces different ideal and empirical non-empty
          islands from mainland extinction", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1
   mainland_clade <- create_test_mainland_clade(mainland_scenario = 4)
   island_spec <- data.frame(
      spec_id = 2,
      main_anc_id = 2,
      col_t = 0.7,
      spec_type = "I",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = as.character(NA))
   mainland_sample_prob <- 1
   mainland_sample_type <- "undiscovered"

   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob,
      mainland_sample_type = mainland_sample_type
   )

   expect_false(identical(island$ideal_island, island$empirical_island))
   expect_equal(island$ideal_island[[1]]$branching_times, c(1.0, 0.3))
   expect_identical(island$ideal_island[[1]]$stac, 4)
   expect_identical(island$ideal_island[[1]]$missing_species, 0)
   expect_identical(island$empirical_island[[1]]$branching_times, c(1.0, 0.5))
   expect_identical(island$empirical_island[[1]]$stac, 2)
   expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("create_island fails with incorrect input", {

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
   island_spec <- create_test_island_spec(island_scenario = 0)

   expect_error(create_island(
      total_time = "nonsense",
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = 1)
   )

   expect_error(create_island(
      total_time = 1,
      island_spec = "nonsense",
      mainland_clade = mainland_clade,
      mainland_sample_prob = 1)
   )

   expect_error(create_island(
      total_time = 1,
      island_spec = island_spec,
      mainland_clade = "nonsense",
      mainland_sample_prob = 1)
   )

   expect_error(create_island(
      total_time = 1,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = "nonsense")
   )
})
