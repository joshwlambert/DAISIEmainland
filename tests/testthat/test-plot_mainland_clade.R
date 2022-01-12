test_that("One species", {
  #
  # --------- 1
  #
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 1)
  plot_mainland_clade(mainland_clade)
  expect_equal(1 + 1, 2) # To prevent testthat 'empty test' warning
})

test_that("Speciation, both offspring species live", {
  #
  #     +----- 2 Cladogensis
  # ----+      1 Extinct
  #     +----- 3 Cladogensis
  #
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
  plot_mainland_clade(mainland_clade)
  expect_equal(1 + 1, 2) # To prevent testthat 'empty test' warning
})

test_that("Speciation, one offspring went extict", {
  #                                                                             # nolint this is no commented code
  #     +---|  2                                                                # nolint this is no commented code
  # ----+      1                                                                # nolint this is no commented code
  #     +----- 3                                                                # nolint this is no commented code
  #                                                                             # nolint this is no commented code
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 4)
  plot_mainland_clade(mainland_clade)
  expect_equal(1 + 1, 2) # To prevent testthat 'empty test' warning
})

test_that("Speciation, both offspring species live", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland <- sim_mainland(
  total_time = 1,
  m = 2,
  mainland_ex = 1)

  plot_mainland_clade(mainland_clade = mainland[[1]])
  plot_mainland_clade(mainland[[2]])
})
