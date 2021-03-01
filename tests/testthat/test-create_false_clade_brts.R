test_that("create_false_clade_brts produces correct output", {
  subset_island <- data.frame(
    spec_id = c(119, 121, 125),
    main_anc_id = c(118, 118, 118),
    col_t_bp = c(0.884864028849, 0.623865697499, 0.406541763485),
    spec_type = c("A", "A", "A"),
    branch_code = c(NA, NA, NA),
    branch_t_bp = c(NA, NA, NA),
    ana_origin = c("clado_extinct", "clado_extinct", "clado_extinct")
  )

  false_clade_brts <- create_false_clade_brts(
    total_time = 1,
    anc_branch_t_bp = 0.950145003459,
    subset_island = subset_island)

  expect_equal(false_clade_brts,
               c(1.00000000000, 0.950145003459, 0.884864028849, 0.623865697499))
})
