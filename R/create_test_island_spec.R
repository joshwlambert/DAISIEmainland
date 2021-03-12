#' Creates an island community for testing purposes
#'
#' @param defaultParamsdoc
#'
#' @return data frame
#' @keywords internal
#'
#' @examples
#' island_spec <- DAISIEmainland:::create_test_island_spec(island_scenario = 1)
create_test_island_spec <- function(island_scenario) {
  if (island_scenario == 1) {
    island_spec <- data.frame(
      spec_id = numeric(),
      main_anc_id = numeric(),
      col_t = numeric(),
      spec_type = character(),
      branch_code = character(),
      branch_t = numeric(),
      ana_origin = character()
    )
  }

  if (island_scenario == 2) {

    island_spec <- data.frame(
      spec_id = 1,
      main_anc_id = 1,
      col_t = 2.413543,
      spec_type = "I",
      branch_code = NA,
      branch_t = NA,
      ana_origin = NA
    )
  }

  if (island_scenario == 3) {
    island_spec <- data.frame(
      spec_id = c(149, 151, 150, 152),
      main_anc_id = c(37, 37, 147, 147),
      col_t = c(0.07551, 0.1226, 0.4587, 0.3716),
      spec_type = rep("A", 4),
      branch_code = rep(NA, 4),
      branch_t = rep(NA, 4),
      ana_origin = rep("immig_parent", 4)
    )
  }

  if (island_scenario == 4) {
    island_spec <- data.frame(
      spec_id = c(149, 151, 148, 152),
      main_anc_id = c(148, 148, 148, 148),
      col_t = c(0.2565, 0.2565, 0.4432, 0.2565),
      spec_type = c("C", "C", "I", "C"),
      branch_code = c("A", "BA", NA, "BB"),
      branch_t = c(0.2565, 0.2622, NA, 1.8829),
      ana_origin = rep(NA, 4)
    )
  }

  if (island_scenario == 5) {
    island_spec <- data.frame(
      spec_id = c(37, 149, 150),
      main_anc_id = c(37, 147, 147),
      col_t = c(0.1154, 0.4123, 0.4123),
      spec_type = c("I", "C", "C"),
      branch_code = c(NA, "A", "B"),
      branch_t = c(NA, 0.4123, 1.2302),
      ana_origin = rep(NA, 3)
    )
  }

  return(island_spec)
}
