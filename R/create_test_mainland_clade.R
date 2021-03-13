#' Creates a clade on the mainland for testing purposes
#'
#' @inheritParams default_params_doc
#'
#' @return data frame
#' @keywords internal
create_test_mainland_clade <- function(mainland_scenario) {
# PN: Improve documentation of mainland_scenario.
  if (mainland_scenario == 1) {
    mainland_clade <- data.frame(
      spec_id = 1,
      main_anc_id = 1,
      spec_type = "I",
      branch_code = "A",
      branch_t = NaN,
      spec_origin_t = 0,
      spec_ex_t = 1.0)
  }

  if (mainland_scenario == 2) {

    mainland_clade <- data.frame(
      spec_id = c(1, 15, 16),
      main_anc_id = c(1, 1, 1),
      spec_type = c("E", "E", "C"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.292906805531114, 0.292906805531114),
      spec_origin_t = c(0, 0.292906805531114, 0.292906805531114),
      spec_ex_t = c(0.292906805531114, 0.541999222479509, 1.0))
  }

  if (mainland_scenario == 3) {

    mainland_clade <- data.frame(
      spec_id = c(2, 27, 28),
      main_anc_id = c(2, 2, 2),
      spec_type = c("E", "C", "C"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.779042070209266, 0.779042070209266),
      spec_origin_t = c(0, 0.779042070209266, 0.779042070209266),
      spec_ex_t = c(0.779042070209266, 1.0, 1.0))
  }

  if (mainland_scenario == 4) {

    mainland_clade <- data.frame(
      spec_id = c(37, 147, 148),
      main_anc_id = c(37, 37, 37),
      spec_type = c("E", "E", "E"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.248564384071771, 0.248564384071771),
      spec_origin_t = c(0, 0.248564384071771, 0.248564384071771),
      spec_ex_t = c(0.248564384071771, 0.502788450052761, 0.502788450052761))
  }

  if (mainland_scenario == 5) {

    mainland_clade <- data.frame(
      spec_id = c(1, 2, 3),
      main_anc_id = c(1, 1, 1),
      spec_type = c("E", "E", "C"),
      branch_code = c("A", "AA", "AB"),
      branch_t = c(NA, 0.5, 0.5),
      spec_origin_t = c(0, 0.5, 0.5),
      spec_ex_t = c(0.5, 0.75, 1.0))
  }

  if (mainland_scenario == 6) {
    mainland_clade <- data.frame(
      spec_id = c(1, 2, 3, 4, 5, 6, 7),
      main_anc_id = c(1, 1, 1, 1, 1, 1, 1),
      spec_type = c("E", "E", "E", "E", "C", "C", "C"),
      branch_code = c("A", "AA", "AB", "AAA", "AAB", "ABA", "ABB"),
      branch_t = c(NA, 0.5, 0.5, 0.6, 0.6, 0.7, 0.7),
      spec_origin_t = c(0, 0.5, 0.5, 0.6, 0.6, 0.7, 0.7),
      spec_ex_t = c(0.5, 0.6, 0.7, 0.8, 1.0, 1.0, 1.0))
  }

  if (mainland_scenario == 7) {
    mainland_clade <- data.frame(
      spec_id = c(1, 2, 3, 4, 5, 6, 7),
      main_anc_id = c(1, 1, 1, 1, 1, 1, 1),
      spec_type = c("E", "E", "E", "E", "E", "C", "C"),
      branch_code = c("A", "AA", "AB", "AAA", "AAB", "ABA", "ABB"),
      branch_t = c(NA, 0.5, 0.5, 0.6, 0.6, 0.7, 0.7),
      spec_origin_t = c(0, 0.5, 0.5, 0.6, 0.6, 0.7, 0.7),
      spec_ex_t = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.0))
  }
  return(mainland_clade)
}
