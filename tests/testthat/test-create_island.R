test_that("empty island is same as ideal", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1
   mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
   island_spec <- create_test_island_spec(island_scenario = 0)
   mainland_sample_prob <- 1

   island <- create_island(
     total_time = total_time,
     island_spec = island_spec,
     mainland_clade = mainland_clade,
     mainland_sample_prob = mainland_sample_prob
   )

   testthat::expect_identical(island$ideal_island, island$empirical_island)
   testthat::expect_identical(island$ideal_island[[1]]$branching_times, 1)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 0)
   testthat::expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("mainland species immigrates and does not go extinct or speciate,
          and no events happen on the island", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
   island_spec <- create_test_island_spec(island_scenario = 1)
   mainland_sample_prob <- 1
   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob
   )
   testthat::expect_identical(island$ideal_island, island$empirical_island)
   testthat::expect_length(island$ideal_island[[1]]$branching_times, 2)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 4)
   testthat::expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("mainland species immigrates to the island then undergoes speciation
          and the descendent species do not go extinct and no events happen on
          the island", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
   island_spec <- create_test_island_spec(island_scenario = 2)
   mainland_sample_prob <- 1
   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob
   )
   testthat::expect_identical(island$ideal_island, island$empirical_island)
   testthat::expect_length(island$ideal_island[[1]]$branching_times, 2)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 2)
   testthat::expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("mainland species undergoes speciation and then one of the descendent
          species immigrates to the island and both the descendent species do
          not go extinct and no events happen on the island", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
   island_spec <- create_test_island_spec(island_scenario = 3)
   mainland_sample_prob <- 1
   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob
   )
   testthat::expect_identical(island$ideal_island, island$empirical_island)
   testthat::expect_length(island$ideal_island[[1]]$branching_times, 2)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 4)
   testthat::expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("mainland species colonises the island and then goes extinct, without
          having speciated, and no events happen on the island", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 8)
   island_spec <- create_test_island_spec(island_scenario = 9)
   mainland_sample_prob <- 1
   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob
   )
   testthat::expect_false(
      identical(island$ideal_island, island$empirical_island)
   )
   testthat::expect_length(island$ideal_island[[1]]$branching_times, 2)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 2)
   testthat::expect_identical(island$ideal_island[[1]]$missing_species, 0)

   testthat::expect_length(island$empirical_island[[1]]$branching_times, 2)
   testthat::expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   testthat::expect_identical(island$empirical_island[[1]]$stac, 5)
   testthat::expect_identical(island$empirical_island[[1]]$missing_species, 0)
})

test_that("mainland species colonises the island and and then undergoes
          speciation and one of the descendant species goes extinct, and no
          events happen on the island", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
   island_spec <- create_test_island_spec(island_scenario = 11)
   mainland_sample_prob <- 1
   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob
   )
   testthat::expect_identical(island$ideal_island, island$empirical_island)
   testthat::expect_length(island$ideal_island[[1]]$branching_times, 2)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 2)
   testthat::expect_identical(island$ideal_island[[1]]$missing_species, 0)
})




test_that("mainland species colonises the island and and then undergoes
          speciation and both of the descendant species goes extinct, and no
          events happen on the island", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 4)
   island_spec <- create_test_island_spec(island_scenario = 9)
   mainland_sample_prob <- 1
   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob
   )
   testthat::expect_false(
      identical(island$ideal_island, island$empirical_island)
   )
   testthat::expect_length(island$ideal_island[[1]]$branching_times, 2)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 2)
   testthat::expect_identical(island$ideal_island[[1]]$missing_species, 0)

   testthat::expect_length(island$empirical_island[[1]]$branching_times, 2)
   testthat::expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   testthat::expect_identical(island$empirical_island[[1]]$stac, 5)
   testthat::expect_identical(island$empirical_island[[1]]$missing_species, 0)
})



test_that("mainland species undergoes speciation and then one of the descendant species immigrates to the island and the other descendant goes extinct and no events happen on the island", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
   island_spec <- create_test_island_spec(island_scenario = 11)
   mainland_sample_prob <- 1
   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob
   )
   testthat::expect_identical(island$ideal_island, island$empirical_island)
   testthat::expect_length(island$ideal_island[[1]]$branching_times, 2)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 2)
   testthat::expect_identical(island$ideal_island[[1]]$missing_species, 0)
})

test_that("mainland ancestor is extinct; multiple colonists same species; no
          clado", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 5)
   island_spec <- create_test_island_spec(island_scenario = 2)
   mainland_sample_prob <- 1

   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob
   )
   testthat::expect_false(
      identical(
         island$ideal_island,
         island$empirical_island
      )
   )
   testthat::expect_gt(length(island$ideal_island[[1]]$branching_times), 1)
   testthat::expect_gt(length(island$empirical_island[[1]]$branching_times), 1)
   testthat::expect_identical(island$empirical_island[[1]]$stac, 6)
   testthat::expect_identical(island$empirical_island[[2]]$stac, 6)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 3)
   testthat::expect_identical(island$ideal_island[[2]]$stac, 3)
   testthat::expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   testthat::expect_identical(
      island$empirical_island[[2]]$branching_times[1],
      island$empirical_island[[2]]$branching_times[2] + 1e-5
   )
   testthat::expect_gt(
      island$ideal_island[[1]]$branching_times[1],
      island$ideal_island[[1]]$branching_times[2]
   )
   testthat::expect_gt(
      island$ideal_island[[2]]$branching_times[1],
      island$ideal_island[[2]]$branching_times[2]
   )
})

test_that("full mainland species are extinct; multiple colonists same species;
          clado event", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 4)

   island_spec <- create_test_island_spec(island_scenario = 4)
   mainland_sample_prob <- 1

   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob
   )
   testthat::expect_false(
      identical(
         island$ideal_island,
         island$empirical_island
      )
   )
   testthat::expect_gt(length(island$ideal_island[[1]]$branching_times), 1)
   testthat::expect_gt(length(island$empirical_island[[1]]$branching_times), 1)

   # ideal has info on recolonist, empirical puts it in same clade
   testthat::expect_lt(
      length(island$empirical_island[[1]]), length(island$ideal_island[[1]])
   )

   testthat::expect_identical(island$empirical_island[[1]]$stac, 6)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 3)
   testthat::expect_identical(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   testthat::expect_gt(
      island$ideal_island[[1]]$branching_times[1],
      island$ideal_island[[1]]$branching_times[2]
   )
})

test_that("mainland ancestor sister is extinct, sister species on the mainland
          is extant, multiple colonists same species; clado event", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 5)
   island_spec <- create_test_island_spec(island_scenario = 7)
   mainland_sample_prob <- 1

   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob
   )
   testthat::expect_false(
      identical(
         island$ideal_island,
         island$empirical_island
      )
   )
   testthat::expect_gt(length(island$ideal_island[[1]]$branching_times), 1)
   testthat::expect_gt(length(island$empirical_island[[1]]$branching_times), 1)

   # ideal has info on recolonist, empirical puts it in same clade
   testthat::expect_lt(
      length(island$empirical_island[[1]]), length(island$ideal_island[[1]])
   )

   testthat::expect_identical(island$empirical_island[[1]]$stac, 2)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 3)
   testthat::expect_gt(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2] + 1e-5
   )
   testthat::expect_gt(
      island$ideal_island[[1]]$branching_times[1],
      island$ideal_island[[1]]$branching_times[2]
   )
})

test_that("mainland ancestor is extinct; only one colonists same species; clade
          and singleton cases", {
   # TODO: check tomorrow; also check: should be "A" in mainland_spec$spec_type?

   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 4)
   island_spec <- create_test_island_spec(island_scenario = 6)
   mainland_sample_prob <- 1

   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob
   )
   testthat::expect_false(
      identical(
         island$ideal_island,
         island$empirical_island
      )
   )
   testthat::expect_gt(length(island$ideal_island[[1]]$branching_times), 1)
   testthat::expect_gt(length(island$empirical_island[[1]]$branching_times), 1)
   # Singleton
   testthat::expect_identical(island$empirical_island[[1]]$stac, 6)

   # Singleton is grafted onto clade on empirical, so only one clade
   testthat::expect_length(island$empirical_island, 1)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 3)

   # Re-colonisations are in object within same level, not as separate clade
   testthat::expect_length(island$ideal_island, 1)

   # Re-colonists take more elements in ideal island
   testthat::expect_lt(
      length(island$empirical_island[[1]]), length(island$ideal_island[[1]])
   )

   testthat::expect_equal(
      island$empirical_island[[1]]$branching_times[1],
      island$empirical_island[[1]]$branching_times[2],
      tolerance = 1e-5
   )
   testthat::expect_gt(
      island$ideal_island[[1]]$branching_times[1],
      island$ideal_island[[1]]$branching_times[2]
   )
})

test_that("mainland ancestor is extant; two colonists same species: one clade
          and one singleton. ancestor speciates after colonisations", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 3)
   island_spec <- create_test_island_spec(island_scenario = 8)
   mainland_sample_prob <- 1

   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob
   )
   testthat::expect_identical(
      island$ideal_island,
      island$empirical_island
   )

   testthat::expect_identical(island$empirical_island[[1]]$stac, 3)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 3)

   testthat::expect_length(island$empirical_island, 1)
   testthat::expect_length(island$ideal_island, 1)
})

test_that("mainland ancestor is extant; two colonists same species: one clade
          and one singleton. ancestor speciates after colonisations, then one
          tip goes extinct", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 5)
   island_spec <- create_test_island_spec(island_scenario = 8)
   mainland_sample_prob <- 1

   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob
   )
   testthat::expect_identical(
      island$ideal_island,
      island$empirical_island
   )

   testthat::expect_identical(island$empirical_island[[1]]$stac, 3)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 3)

   testthat::expect_length(island$empirical_island, 1)
   testthat::expect_length(island$ideal_island, 1)
})

test_that("mainland ancestor is extant; two colonists same species: one
          clade and one singleton. ancestor speciates after colonisations,
          then all tips goes extinct", {
   set.seed(
      1,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection")
   total_time <- 1

   mainland_clade <- create_test_mainland_clade(mainland_scenario = 5)
   island_spec <- create_test_island_spec(island_scenario = 8)
   mainland_sample_prob <- 1

   island <- create_island(
      total_time = total_time,
      island_spec = island_spec,
      mainland_clade = mainland_clade,
      mainland_sample_prob = mainland_sample_prob
   )
   testthat::expect_identical(
      island$ideal_island,
      island$empirical_island
   )

   testthat::expect_identical(island$empirical_island[[1]]$stac, 3)
   testthat::expect_identical(island$ideal_island[[1]]$stac, 3)

   testthat::expect_length(island$empirical_island, 1)
   testthat::expect_length(island$ideal_island, 1)
})
