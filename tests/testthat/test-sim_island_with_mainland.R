test_that("sim_island_with_mainland is silent and produces correct empty island", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  expect_silent(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ext = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = FALSE)
  )
  expect_equal(island$ideal_islands[[1]][[1]]$island_age, 1)
  expect_equal(island$ideal_islands[[1]][[1]]$not_present, 10)
  expect_equal(island$empirical_islands[[1]][[1]]$island_age, 1)
  expect_equal(island$empirical_islands[[1]][[1]]$not_present, 10)

})

# test_that("sim_island_mainland is silent and produces correct non-empty island", {
#    set.seed(
# 1,
# kind = "Mersenne-Twister",
# normal.kind = "Inversion",
# sample.kind = "Rejection"
# )
#    expect_silent(
#     island <- sim_island_with_mainland(
#       time = 1,
#       m = 100,
#       island_pars = c(1, 1, 10, 1, 1),
#       mainland_ext = 1,
#       mainland_sample_prob = 1,
#       replicates = 1,
#       verbose = FALSE)
#   )
#   expect_equal(island$ideal_island[[1]]$branching_times,
#                c(1.000000000000, 0.949774116209, 0.230878289967))
#   expect_equal(island$ideal_island[[1]]$stac, 2)
#   expect_equal(island$ideal_island[[1]]$missing_species, 0)
#   expect_equal(island$ideal_island[[2]]$branching_times,
#                c(1.0000000000000, 0.0116755987724))
#   expect_equal(island$ideal_island[[2]]$stac, 4)
#   expect_equal(island$ideal_island[[2]]$missing_species, 0)
#   expect_equal(island$empirical_island[[1]]$branching_times,
#                c(1.000000000000, 0.949774116209, 0.230878289967))
#   expect_equal(island$empirical_island[[1]]$stac, 2)
#   expect_equal(island$empirical_island[[1]]$missing_species, 0)
#   expect_equal(island$empirical_island[[2]]$branching_times,
#                c(1.0000000000000, 0.0116755987724))
#   expect_equal(island$empirical_island[[2]]$stac, 4)
#   expect_equal(island$empirical_island[[2]]$missing_species, 0)
# })

test_that("sim_island_mainland fails with incorrect input", {
  expect_error(island <- sim_island_with_mainland(
    time = "nonsense",
    m = 10,
    island_pars = c(1,1,10,1,1),
    mainland_ext = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = "nonsense",
    island_pars = c(1,1,10,1,1),
    mainland_ext = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = "nonsense",
    mainland_ext = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1,1,10,1,1),
    mainland_ext = "nonsense",
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1,1,10,1,1),
    mainland_ext = 1,
    mainland_sample_prob = "nonsense",
    replicates = 1,
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1,1,10,1,1),
    mainland_ext = 1,
    mainland_sample_prob = 1,
    replicates = "nonsense",
    verbose = TRUE)
  )

  expect_error(island <- sim_island_with_mainland(
    time = 1,
    m = 10,
    island_pars = c(1,1,10,1,1),
    mainland_ext = 1,
    mainland_sample_prob = 1,
    replicates = 1,
    verbose = "nonsense")
  )
})
