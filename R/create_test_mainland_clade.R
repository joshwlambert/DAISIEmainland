#' Creates a clade on the mainland for testing purposes
#'
#' @inheritParams default_params_doc
#'
#' @return data frame with mainland clade information
#' @keywords internal
create_test_mainland_clade <- function(mainland_scenario) {

  testit::assert(mainland_scenario >= 1 && mainland_scenario <= 14)

  if (mainland_scenario == 1) {
    # Single species (regular DAISIE)
    mainland_clade <- data.frame(
      spec_id = 1,
      main_anc_id = 1,
      spec_type = "I",
      branch_code = "A",
      branch_t = NaN,
      spec_origin_t = 0,
      spec_ex_t = 1.0
    )
  }

  if (mainland_scenario == 2) {
    # 1 speciation no extinction
    mainland_clade <- data.frame(
      spec_id = 1:3,
      main_anc_id = rep(1, 3),
      spec_type = c("E", "C", "C"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.5, 0.5),
      spec_origin_t = c(0, 0.5, 0.5),
      spec_ex_t = c(0.5, 1.0, 1.0)
    )
  }

  if (mainland_scenario == 3) {
    # Extinct, no speciation
    mainland_clade <- data.frame(
      spec_id = 1,
      main_anc_id = 1,
      spec_type = "E",
      branch_code = "A",
      branch_t = NaN,
      spec_origin_t = 0,
      spec_ex_t = 0.5)
  }

  if (mainland_scenario == 4) {
    # 1 speciation, 1 extinction
    mainland_clade <- data.frame(
      spec_id = 1:3,
      main_anc_id = rep(1, 3),
      spec_type = c("E", "E", "C"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.5, 0.5),
      spec_origin_t = c(0, 0.5, 0.5),
      spec_ex_t = c(0.5, 0.75, 1.0)
    )
  }

  if (mainland_scenario == 5) {
    # 1 speciation, 2 extinctions
    mainland_clade <- data.frame(
      spec_id = 1:3,
      main_anc_id = rep(1, 3),
      spec_type = c("E", "E", "E"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.5, 0.5),
      spec_origin_t = c(0, 0.5, 0.5),
      spec_ex_t = c(0.5, 0.75, 0.75)
    )
  }

  if (mainland_scenario == 6) {
    # 1 speciation, 1 extinction (other branch)
    mainland_clade <- data.frame(
      spec_id = 1:3,
      main_anc_id = rep(1, 3),
      spec_type = c("E", "C", "E"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.5, 0.5),
      spec_origin_t = c(0, 0.5, 0.5),
      spec_ex_t = c(0.5, 1.00, 0.75)
    )
  }

  if (mainland_scenario == 7) {
    # 2 speciations no extinction
    mainland_clade <- data.frame(
      spec_id = 1:5,
      main_anc_id = rep(1, 5),
      spec_type = c("E", "E", "C", "C", "C"),
      branch_code = c("A", "AA", "AB", "AAA", "AAB"),
      branch_t = c(NA, 0.5, 0.5, 0.6, 0.6),
      spec_origin_t = c(0, 0.5, 0.5, 0.6, 0.6),
      spec_ex_t = c(0.5, 0.6, 1.0, 1.0, 1.0)
    )
  }

  if (mainland_scenario == 8) {
    # 2 speciations 1 extinction (other branch)
    mainland_clade <- data.frame(
      spec_id = 1:5,
      main_anc_id = rep(1, 5),
      spec_type = c("E", "E", "C", "C", "E"),
      branch_code = c("A", "AA", "AB", "AAA", "AAB"),
      branch_t = c(NA, 0.5, 0.5, 0.6, 0.6),
      spec_origin_t = c(0, 0.5, 0.5, 0.6, 0.6),
      spec_ex_t = c(0.5, 0.6, 1.0, 1.0, 0.75)
    )
  }

  if (mainland_scenario == 9) {
    # 2 speciations 1 extinction
    mainland_clade <- data.frame(
      spec_id = 1:5,
      main_anc_id = rep(1, 5),
      spec_type = c("E", "E", "C", "E", "C"),
      branch_code = c("A", "AA", "AB", "AAA", "AAB"),
      branch_t = c(NA, 0.5, 0.5, 0.6, 0.6),
      spec_origin_t = c(0, 0.5, 0.5, 0.6, 0.6),
      spec_ex_t = c(0.5, 0.6, 1.0, 0.75, 1.0)
    )
  }

  if (mainland_scenario == 10) {
    # 2 speciations 2 extinction
    mainland_clade <- data.frame(
      spec_id = 1:5,
      main_anc_id = rep(1, 5),
      spec_type = c("E", "E", "C", "E", "E"),
      branch_code = c("A", "AA", "AB", "AAA", "AAB"),
      branch_t = c(NA, 0.5, 0.5, 0.6, 0.6),
      spec_origin_t = c(0, 0.5, 0.5, 0.6, 0.6),
      spec_ex_t = c(0.5, 0.6, 1.0, 0.75, 0.75)
    )
  }

  if (mainland_scenario == 11) {
    # 1 speciation, 2 extinctions, anagenesis on the island before speciation
    # on the mainland
    mainland_clade <- data.frame(
      spec_id = c(1, 3, 4),
      main_anc_id = rep(1, 3),
      spec_type = c("E", "E", "E"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.5, 0.5),
      spec_origin_t = c(0, 0.5, 0.5),
      spec_ex_t = c(0.5, 0.75, 0.75)
    )
  }

  if (mainland_scenario == 12) {
    # Late extinct, no speciation
    mainland_clade <- data.frame(
      spec_id = 1,
      main_anc_id = 1,
      spec_type = "E",
      branch_code = "A",
      branch_t = NaN,
      spec_origin_t = 0,
      spec_ex_t = 0.8)
  }

  if (mainland_scenario == 13) {
    # 1 late speciation no extinction
    mainland_clade <- data.frame(
      spec_id = 1:3,
      main_anc_id = rep(1, 3),
      spec_type = c("E", "C", "C"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.8, 0.8),
      spec_origin_t = c(0, 0.8, 0.8),
      spec_ex_t = c(0.8, 1.0, 1.0)
    )
  }

  if (mainland_scenario == 14) {
    # 1 late speciation, 1 extinction
    mainland_clade <- data.frame(
      spec_id = 1:3,
      main_anc_id = rep(1, 3),
      spec_type = c("E", "E", "C"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.8, 0.8),
      spec_origin_t = c(0, 0.8, 0.8),
      spec_ex_t = c(0.8, 0.95, 1.0)
    )
  }

  return(mainland_clade)
}
