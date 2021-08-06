test_that("common_ancestor_time produces correct output", {
  mainland_clade <- data.frame(
    spec_id = c(35, 207, 208, 239, 240),
    main_anc_id = c(35, 35, 35, 35, 35),
    spec_type = c("E", "E", "E", "C", "E"),
    branch_code = c("A", "AA", "AB", "AAA", "AAB"),
    branch_t = c(NaN,
                 0.5289956039,
                 0.5289956039,
                0.7015891307,
                0.7015891307),
    spec_origin_t = c(0, 0.5289956039,
                      0.5289956039,
                      0.7015891307,
                      0.7015891307),
    spec_ex_t = c(0.5289956039,
                  0.7015891307,
                  0.8831928636,
                  1.0000000000,
                  0.9367214999)
  )

  branching_t <- common_ancestor_time(
    total_time = 1.0,
    mainland_spec = 2,
    mainland_clade = mainland_clade)

  expect_equal(branching_t, 0.2984108693)

})
