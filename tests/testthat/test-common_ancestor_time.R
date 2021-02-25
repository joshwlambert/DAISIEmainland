test_that("common_ancestor_time produces correct output for mainland_clade 5", {
  # RJCB: Awesome!
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 5)

  branching_t <- common_ancestor_time(
    total_time = 1.0,
    mainland_spec = 2,
    mainland_clade = mainland_clade)

  expect_equal(branching_t, 0.5)

})

test_that("common_ancestor_time produces correct output for mainland_clade 6", {
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 6)

  branching_t <- common_ancestor_time(
    total_time = 1.0,
    mainland_spec = 4,
    mainland_clade = mainland_clade)

  expect_equal(branching_t, 0.4)
})


test_that("common_ancestor_time produces correct output for mainland_clade 7", {
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 7)

  branching_t <- common_ancestor_time(
    total_time = 1.0,
    mainland_spec = 4,
    mainland_clade = mainland_clade)

  expect_equal(branching_t, 0.5)
})

