#' Creates a clade on the mainland for testing purposes
#'
#' @inheritParams default_params_doc
#'
#' @return data frame with mainland clade information
#' @keywords internal
#' @author Joshua W. Lambert
create_test_mainland_clade <- function(mainland_scenario) {

  testit::assert(mainland_scenario >= 1 && mainland_scenario <= 24)

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
  } else if (mainland_scenario == 2) {
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
  } else if (mainland_scenario == 3) {
    # Extinct, no speciation
    mainland_clade <- data.frame(
      spec_id = 1,
      main_anc_id = 1,
      spec_type = "E",
      branch_code = "A",
      branch_t = NaN,
      spec_origin_t = 0,
      spec_ex_t = 0.67)
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
      spec_ex_t = c(0.5, 0.83, 1.0)
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
      spec_ex_t = c(0.5, 0.83, 0.83)
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
      spec_ex_t = c(0.5, 1.00, 0.83)
    )
  }

  if (mainland_scenario == 7) {
    # 1 speciation (after island), 1 extinction
    mainland_clade <- data.frame(
      spec_id = c(1, 3, 4),
      main_anc_id = rep(1, 3),
      spec_type = c("E", "E", "C"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.5, 0.5),
      spec_origin_t = c(0, 0.5, 0.5),
      spec_ex_t = c(0.5, 0.75, 1.0)
    )
  }

  if (mainland_scenario == 8) {
    # 1 speciation (after island), 2 extinctions
    mainland_clade <- data.frame(
      spec_id = c(1, 3, 4),
      main_anc_id = rep(1, 3),
      spec_type = c("E", "E", "E"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.5, 0.5),
      spec_origin_t = c(0, 0.5, 0.5),
      spec_ex_t = c(0.5, 0.67, 0.67)
    )
  }

  if (mainland_scenario == 9) {
    # 1 speciation, 2 extinctions (early)
    mainland_clade <- data.frame(
      spec_id = 1:3,
      main_anc_id = rep(1, 3),
      spec_type = c("E", "E", "E"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.5, 0.5),
      spec_origin_t = c(0, 0.5, 0.5),
      spec_ex_t = c(0.5, 0.67, 0.67)
    )
  }

  if (mainland_scenario == 10) {
    # 1 speciation, 1 extinction (early)
    mainland_clade <- data.frame(
      spec_id = 1:3,
      main_anc_id = rep(1, 3),
      spec_type = c("E", "E", "C"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.5, 0.5),
      spec_origin_t = c(0, 0.5, 0.5),
      spec_ex_t = c(0.5, 0.67, 1.0)
    )
  }

  if (mainland_scenario == 11) {
    # 1 speciation (late) no extinction
    mainland_clade <- data.frame(
      spec_id = c(1, 4, 5),
      main_anc_id = rep(1, 3),
      spec_type = c("E", "C", "C"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.67, 0.67),
      spec_origin_t = c(0, 0.67, 0.67),
      spec_ex_t = c(0.67, 1.0, 1.0)
    )
  }

  if (mainland_scenario == 12) {
    # 1 speciation (late), 1 extinction (late)
    mainland_clade <- data.frame(
      spec_id = c(1, 4, 5),
      main_anc_id = rep(1, 3),
      spec_type = c("E", "E", "C"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.67, 0.67),
      spec_origin_t = c(0, 0.67, 0.67),
      spec_ex_t = c(0.5, 0.83, 1.0)
    )
  }

  if (mainland_scenario == 13) {
    # 1 speciation (late), 2 extinctions (late)
    mainland_clade <- data.frame(
      spec_id = c(1, 4, 5),
      main_anc_id = rep(1, 3),
      spec_type = c("E", "E", "E"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.67, 0.67),
      spec_origin_t = c(0, 0.67, 0.67),
      spec_ex_t = c(0.67, 0.83, 0.83)
    )
  }

  if (mainland_scenario == 14) {
    # 1 speciation (early) no extinction
    mainland_clade <- data.frame(
      spec_id = 1:3,
      main_anc_id = rep(1, 3),
      spec_type = c("E", "C", "C"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.16, 0.16),
      spec_origin_t = c(0, 0.16, 0.16),
      spec_ex_t = c(0.16, 1.0, 1.0)
    )
  }

  if (mainland_scenario == 15) {
    # 1 speciation (early), 1 extinction
    mainland_clade <- data.frame(
      spec_id = 1:3,
      main_anc_id = rep(1, 3),
      spec_type = c("E", "C", "E"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.16, 0.16),
      spec_origin_t = c(0, 0.16, 0.16),
      spec_ex_t = c(0.16, 1.0, 0.83)
    )
  }

  if (mainland_scenario == 16) {
    # 1 speciation (early), 2 extinctions
    mainland_clade <- data.frame(
      spec_id = 1:3,
      main_anc_id = rep(1, 3),
      spec_type = c("E", "E", "E"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.16, 0.16),
      spec_origin_t = c(0, 0.16, 0.16),
      spec_ex_t = c(0.16, 0.83, 0.83)
    )
  }

  if (mainland_scenario == 17) {
    # 2 speciations no extinction
    mainland_clade <- data.frame(
      spec_id = 1:5,
      main_anc_id = rep(1, 5),
      spec_type = c("E", "E", "C", "C", "C"),
      branch_code = c("A", "AA", "AB", "AAA", "AAB"),
      branch_t = c(NA, 0.33, 0.33, 0.5, 0.5),
      spec_origin_t = c(0, 0.33, 0.33, 0.5, 0.5),
      spec_ex_t = c(0.33, 0.5, 1.0, 1.0, 1.0)
    )
  }

  if (mainland_scenario == 18) {
    # 2 speciations 1 extinction
    mainland_clade <- data.frame(
      spec_id = 1:5,
      main_anc_id = rep(1, 5),
      spec_type = c("E", "E", "C", "C", "E"),
      branch_code = c("A", "AA", "AB", "AAA", "AAB"),
      branch_t = c(NA, 0.33, 0.33, 0.5, 0.5),
      spec_origin_t = c(0, 0.33, 0.33, 0.5, 0.5),
      spec_ex_t = c(0.33, 0.5, 1.0, 1.0, 0.83)
    )
  }

  if (mainland_scenario == 19) {
    # 2 speciations 1 extinction (other branch)
    mainland_clade <- data.frame(
      spec_id = 1:5,
      main_anc_id = rep(1, 5),
      spec_type = c("E", "E", "C", "E", "C"),
      branch_code = c("A", "AA", "AB", "AAA", "AAB"),
      branch_t = c(NA, 0.33, 0.33, 0.5, 0.5),
      spec_origin_t = c(0, 0.33, 0.33, 0.5, 0.5),
      spec_ex_t = c(0.33, 0.5, 1.0, 0.83, 1.0)
    )
  }

  if (mainland_scenario == 20) {
    # 2 speciations 2 extinction
    mainland_clade <- data.frame(
      spec_id = 1:5,
      main_anc_id = rep(1, 5),
      spec_type = c("E", "E", "C", "E", "E"),
      branch_code = c("A", "AA", "AB", "AAA", "AAB"),
      branch_t = c(NA, 0.33, 0.33, 0.5, 0.5),
      spec_origin_t = c(0, 0.33, 0.33, 0.5, 0.5),
      spec_ex_t = c(0.33, 0.5, 1.0, 0.83, 0.83)
    )
  }

  return(mainland_clade)
}
