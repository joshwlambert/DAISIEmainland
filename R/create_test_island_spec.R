#' Creates an island community for testing purposes
#'
#' @param defaultParamsdoc
#'
#' @return data frame with island community information
#' @keywords internal
create_test_island_spec <- function(island_scenario) {

  if (island_scenario == 1) {
    island_spec <- data.frame(
      spec_id = numeric(),
      main_anc_id = numeric(),
      col_t = numeric(),
      spec_type = character(),
      branch_code = character(),
      branch_t = numeric(),
      ana_origin = character())
  }

  if (island_scenario == 2) {

    island_spec <- data.frame(
      spec_id = 1,
      main_anc_id = 1,
      col_t = 2.413543,
      spec_type = "I",
      branch_code = NA,
      branch_t = NA,
      ana_origin = NA)
  }

  if (island_scenario == 3) {

    island_spec <- data.frame(
      spec_id = c(149, 151, 150, 152),
      main_anc_id = c(37, 37, 147, 147),
      col_t = c(0.07551, 0.1226, 0.4587, 0.3716),
      spec_type = c("A", "A", "A", "A"),
      branch_code = as.character(c(NA, NA, NA, NA)),
      branch_t = c(NaN, NaN, NaN, NaN),
      ana_origin = c(
        "immig_parent",
        "immig_parent",
        "immig_parent",
        "immig_parent"))
  }

  if (island_scenario == 4) {

    island_spec <- data.frame(
      spec_id = c(149, 151, 148, 152),
      main_anc_id = c(148, 148, 148, 148),
      col_t = c(0.2565, 0.2565, 0.4432, 0.2565),
      spec_type = c("C", "C", "I", "C"),
      branch_code = c("A", "AA", NA, "AB"),
      branch_t = c(0.2565, 0.2622, NA, 1.8829),
      ana_origin = rep(NA, 4))
  }

  if (island_scenario == 5) {

    island_spec <- data.frame(
      spec_id = c(37, 149, 150),
      main_anc_id = c(37, 147, 147),
      col_t = c(0.1154, 0.4123, 0.4123),
      spec_type = c("I", "C", "C"),
      branch_code = c(NA, "A", "B"),
      branch_t = c(NA, 0.4123, 1.2302),
      ana_origin = as.character(c(NA, NA, NA))
    )
  }

  if (island_scenario == 6) {

    island_spec <- data.frame(
      spec_id = c(3, 4, 5, 2),
      main_anc_id = c(2, 2, 2, 2),
      col_t = c(0.55, 0.55, 0.55, 0.65),
      spec_type = c("C", "C", "C", "I"),
      branch_code = c(NA, "A", "B", NA),
      branch_t = c(0.55, 0.55, 0.6, NA),
      ana_origin = c(NA, NA, NA, NA))
  }

  if (island_scenario == 7) {

    island_spec <- data.frame(
      spec_id = c(4, 5, 2),
      main_anc_id = c(2, 2, 2),
      col_t = c(0.55, 0.55, 0.70),
      spec_type = c("C", "C", "I"),
      branch_code = c("A", "B", as.character(NA)),
      branch_t = c(0.55, 0.60, NaN),
      ana_origin = as.character(c(NA, NA, NA)))
  }
  return(island_spec)
}
