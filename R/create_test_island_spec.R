#' Creates an island community for testing purposes
#'
#' @inheritParams default_params_doc
#'
#' @return data frame with island community information
#' @keywords internal
#' @author Joshua W. Lambert
create_test_island_spec <- function(island_scenario) {

  testit::assert(island_scenario >= 0 && island_scenario <= 54)

  if (island_scenario == 0) {
    # Empty island
    island_spec <- data.frame(
      spec_id = numeric(),
      main_anc_id = numeric(),
      col_t = numeric(),
      spec_type = character(),
      branch_code = character(),
      branch_t = numeric(),
      ana_origin = character())
  }

  if (island_scenario == 1) {
    # Single lineage non-endemic (early col)
    island_spec <- data.frame(
      spec_id = 1,
      main_anc_id = 1,
      col_t = 0.16,
      spec_type = "I",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = as.character(NA))
  }

  if (island_scenario == 2) {
    # Single lineage endemic (early col)
    island_spec <- data.frame(
      spec_id = 1,
      main_anc_id = 1,
      col_t = 0.16,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "mainland_extinction")
  }

  if (island_scenario == 3) {
    # Single lineage non-endemic (late col)
    island_spec <- data.frame(
      spec_id = 2,
      main_anc_id = 2,
      col_t = 0.67,
      spec_type = "I",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = as.character(NA))
  }

  if (island_scenario == 4) {
    # Single lineage endemic (early col)
    island_spec <- data.frame(
      spec_id = 1,
      main_anc_id = 1,
      col_t = 0.16,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "mainland_extinction")
  }

  if (island_scenario == 5) {
    # Single lineage endemic (early col)
    island_spec <- data.frame(
      spec_id = 1,
      main_anc_id = 1,
      col_t = 0.16,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "mainland_extinction")
  }

  if (island_scenario == 6) {
    # Single lineage endemic (early col)
    island_spec <- data.frame(
      spec_id = 1,
      main_anc_id = 1,
      col_t = 0.16,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "mainland_extinction")
  }

  if (island_scenario == 7) {
    # Single lineage non-endemic (late col)
    island_spec <- data.frame(
      spec_id = 3,
      main_anc_id = 3,
      col_t = 0.67,
      spec_type = "I",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = as.character(NA))
  }

  if (island_scenario == 8) {
    # Single lineage endemic (late col)
    island_spec <- data.frame(
      spec_id = 3,
      main_anc_id = 3,
      col_t = 0.67,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "mainland_extinction")
  }

  if (island_scenario == 9) {
    # Single lineage endemic (late col)
    island_spec <- data.frame(
      spec_id = 3,
      main_anc_id = 3,
      col_t = 0.67,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "mainland_extinction")
  }

  if (island_scenario == 10) {
    # Single lineage endemic (early col)
    island_spec <- data.frame(
      spec_id = 2,
      main_anc_id = 1,
      col_t = 0.16,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "immig_parent")
  }

  if (island_scenario == 11) {
    # Single lineage endemic (early col)
    island_spec <- data.frame(
      spec_id = 2,
      main_anc_id = 1,
      col_t = 0.16,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "immig_parent")
  }

  if (island_scenario == 12) {
    # Single lineage endemic (late col)
    island_spec <- data.frame(
      spec_id = 4,
      main_anc_id = 2,
      col_t = 0.67,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "immig_parent")
  }

  if (island_scenario == 13) {
    # Single lineage endemic (early col)
    island_spec <- data.frame(
      spec_id = 2,
      main_anc_id = 1,
      col_t = 0.16,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "immig_parent")
  }

  if (island_scenario == 14) {
    # Single lineage endemic (early col)
    island_spec <- data.frame(
      spec_id = 2,
      main_anc_id = 1,
      col_t = 0.16,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "immig_parent")
  }

  if (island_scenario == 15) {
    # Single lineage endemic (early col)
    island_spec <- data.frame(
      spec_id = 2,
      main_anc_id = 1,
      col_t = 0.16,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "immig_parent")
  }

  if (island_scenario == 16) {
    # Single lineage endemic (late col)
    island_spec <- data.frame(
      spec_id = 4,
      main_anc_id = 3,
      col_t = 0.67,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "immig_parent")
  }

  if (island_scenario == 17) {
    # Single lineage endemic (late col)
    island_spec <- data.frame(
      spec_id = 4,
      main_anc_id = 3,
      col_t = 0.67,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "immig_parent")
  }

  if (island_scenario == 18) {
    # Single lineage endemic (late col)
    island_spec <- data.frame(
      spec_id = 4,
      main_anc_id = 3,
      col_t = 0.67,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = "immig_parent")
    #should this be "immig_parent" or "mainland_extinction"?
  }

  if (island_scenario == 19) {
    # 2 species clade (early col)
    island_spec <- data.frame(
      spec_id = c(2, 3),
      main_anc_id = c(1, 1),
      col_t = c(0.16, 0.16),
      spec_type = c("C", "C"),
      branch_code = c("A", "B"),
      branch_t = c(0.16, 0.5),
      ana_origin = c(as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 20) {
    # 2 species clade (early col)
    island_spec <- data.frame(
      spec_id = c(4, 5),
      main_anc_id = c(1, 1),
      col_t = c(0.16, 0.16),
      spec_type = c("C", "C"),
      branch_code = c("A", "B"),
      branch_t = c(0.16, 0.58),
      ana_origin = c(as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 21) {
    # 2 species clade (early col & early speciation)
    island_spec <- data.frame(
      spec_id = c(2, 3),
      main_anc_id = c(1, 1),
      col_t = c(0.16, 0.16),
      spec_type = c("C", "C"),
      branch_code = c("A", "B"),
      branch_t = c(0.16, 0.5),
      ana_origin = c(as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 22) {
    # 2 species clade (late col)
    island_spec <- data.frame(
      spec_id = c(4, 5),
      main_anc_id = c(3, 3),
      col_t = c(0.67, 0.67),
      spec_type = c("C", "C"),
      branch_code = c("A", "B"),
      branch_t = c(0.67, 0.83),
      ana_origin = c(as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 23) {
    # 2 species clade (early col)
    island_spec <- data.frame(
      spec_id = c(4, 5),
      main_anc_id = c(1, 1),
      col_t = c(0.16, 0.16),
      spec_type = c("C", "C"),
      branch_code = c("A", "B"),
      branch_t = c(0.16, 0.58),
      ana_origin = c(as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 24) {
    # 2 species clade (early col)
    island_spec <- data.frame(
      spec_id = c(4, 5),
      main_anc_id = c(1, 1),
      col_t = c(0.16, 0.16),
      spec_type = c("C", "C"),
      branch_code = c("A", "B"),
      branch_t = c(0.16, 0.58),
      ana_origin = c(as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 25) {
    # 2 species clade (late col)
    island_spec <- data.frame(
      spec_id = c(4, 5),
      main_anc_id = c(3, 3),
      col_t = c(0.67, 0.67),
      spec_type = c("C", "C"),
      branch_code = c("A", "B"),
      branch_t = c(0.67, 0.83),
      ana_origin = c(as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 26) {
    # 2 species clade (late col)
    island_spec <- data.frame(
      spec_id = c(4, 5),
      main_anc_id = c(3, 3),
      col_t = c(0.67, 0.67),
      spec_type = c("C", "C"),
      branch_code = c("A", "B"),
      branch_t = c(0.67, 0.83),
      ana_origin = c(as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 27) {
    # 2 species clade (late col)
    island_spec <- data.frame(
      spec_id = c(4, 5),
      main_anc_id = c(3, 3),
      col_t = c(0.67, 0.67),
      spec_type = c("C", "C"),
      branch_code = c("A", "B"),
      branch_t = c(0.67, 0.91),
      ana_origin = c(as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 28) {
    # 2 species clade (early col) & and non-endemic recol
    island_spec <- data.frame(
      spec_id = c(2, 3, 1),
      main_anc_id = c(1, 1, 1),
      col_t = c(0.16, 0.16, 0.5),
      spec_type = c("C", "C", "I"),
      branch_code = c("A", "B", NA),
      branch_t = c(0.16, 0.33, NaN),
      ana_origin = c(as.character(NA),
                     as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 29) {
    # 2 species clade (early col) & and endemic recol
    island_spec <- data.frame(
      spec_id = c(2, 3, 1),
      main_anc_id = c(1, 1, 1),
      col_t = c(0.16, 0.16, 0.5),
      spec_type = c("C", "C", "A"),
      branch_code = c("A", "B", as.character(NA)),
      branch_t = c(0.16, 0.33, NaN),
      ana_origin = c(as.character(NA),
                     as.character(NA),
                     "mainland_extinction"))
  }

  if (island_scenario == 30) {
    # 2 species clade (early col) & and endemic recol
    island_spec <- data.frame(
      spec_id = c(2, 3, 1),
      main_anc_id = c(1, 1, 1),
      col_t = c(0.16, 0.16, 0.5),
      spec_type = c("C", "C", "A"),
      branch_code = c("A", "B", as.character(NA)),
      branch_t = c(0.16, 0.33, NaN),
      ana_origin = c(as.character(NA),
                     as.character(NA),
                     "mainland_extinction"))
  }

  if (island_scenario == 31) {
    # 2 species clade (early col) & and endemic recol
    island_spec <- data.frame(
      spec_id = c(2, 3, 1),
      main_anc_id = c(1, 1, 1),
      col_t = c(0.16, 0.16, 0.5),
      spec_type = c("C", "C", "A"),
      branch_code = c("A", "B", as.character(NA)),
      branch_t = c(0.16, 0.33, NaN),
      ana_origin = c(as.character(NA),
                     as.character(NA),
                     "mainland_extinction"))
  }

  if (island_scenario == 32) {
    # 2 species clade (early col) & and endemic recol
    island_spec <- data.frame(
      spec_id = c(2, 3, 1),
      main_anc_id = c(1, 1, 1),
      col_t = c(0.16, 0.16, 0.5),
      spec_type = c("C", "C", "A"),
      branch_code = c("A", "B", as.character(NA)),
      branch_t = c(0.16, 0.33, NaN),
      ana_origin = c(as.character(NA),
                     as.character(NA),
                     "mainland_extinction"))
  }

  if (island_scenario == 33) {
    # 2 species clade (early col) & and non-endemic recol
    island_spec <- data.frame(
      spec_id = c(4, 5, 3),
      main_anc_id = c(3, 3, 3),
      col_t = c(0.33, 0.33, 0.67),
      spec_type = c("C", "C", "I"),
      branch_code = c("A", "B", as.character(NA)),
      branch_t = c(0.33, 0.5, NaN),
      ana_origin = c(as.character(NA),
                     as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 34) {
    # 2 species clade (early col) & and endemic recol
    island_spec <- data.frame(
      spec_id = c(4, 5, 3),
      main_anc_id = c(3, 3, 3),
      col_t = c(0.33, 0.33, 0.67),
      spec_type = c("C", "C", "A"),
      branch_code = c("A", "B", as.character(NA)),
      branch_t = c(0.33, 0.5, NaN),
      ana_origin = c(as.character(NA),
                     as.character(NA),
                     "mainland_extinction"))
  }

  if (island_scenario == 35) {
    # 2 species clade (early col) & and endemic recol
    island_spec <- data.frame(
      spec_id = c(4, 5, 3),
      main_anc_id = c(3, 3, 3),
      col_t = c(0.33, 0.33, 0.67),
      spec_type = c("C", "C", "A"),
      branch_code = c("A", "B", as.character(NA)),
      branch_t = c(0.33, 0.5, NaN),
      ana_origin = c(as.character(NA),
                     as.character(NA),
                     "mainland_extinction"))
  }

  if (island_scenario == 36) {
    # Single lineage endemic (early col) & single lineage non-endemic (late col)
    island_spec <- data.frame(
      spec_id = c(1, 3),
      main_anc_id = c(1, 3),
      col_t = c(0.16, 0.67),
      spec_type = c("A", "I"),
      branch_code = c(as.character(NA),
                      as.character(NA)),
      branch_t = c(NaN, NaN),
      ana_origin = c("mainland_extinction",
                     as.character(NA)))
  }

  if (island_scenario == 37) {
    # Single lineage endemic (early col) & single lineage non-endemic (late col)
    island_spec <- data.frame(
      spec_id = c(1, 3),
      main_anc_id = c(1, 3),
      col_t = c(0.16, 0.67),
      spec_type = c("A", "I"),
      branch_code = c(as.character(NA),
                      as.character(NA)),
      branch_t = c(NaN, NaN),
      ana_origin = c("mainland_extinction",
                     as.character(NA)))
  }

  if (island_scenario == 38) {
    # Single lineage endemic (early col) & single lineage endemic (late col)
    island_spec <- data.frame(
      spec_id = c(1, 3),
      main_anc_id = c(1, 3),
      col_t = c(0.16, 0.67),
      spec_type = c("A", "A"),
      branch_code = c(as.character(NA),
                      as.character(NA)),
      branch_t = c(NaN, NaN),
      ana_origin = c("mainland_extinction",
                     "mainland_extinction"))
  }

  if (island_scenario == 39) {
    # Single lineage endemic (early col) & single lineage endemic (late col)
    island_spec <- data.frame(
      spec_id = c(1, 3),
      main_anc_id = c(1, 3),
      col_t = c(0.16, 0.67),
      spec_type = c("A", "A"),
      branch_code = c(as.character(NA),
                      as.character(NA)),
      branch_t = c(NaN, NaN),
      ana_origin = c("mainland_extinction",
                     "mainland_extinction"))
  }

  if (island_scenario == 40) {
    # Single lineage endemic (early col) & single lineage non-endemic (late col)
    island_spec <- data.frame(
      spec_id = c(2, 4),
      main_anc_id = c(2, 4),
      col_t = c(0.42, 0.67),
      spec_type = c("A", "I"),
      branch_code = c(as.character(NA),
                      as.character(NA)),
      branch_t = c(NaN, NaN),
      ana_origin = c("mainland_extinction",
                     as.character(NA)))
  }

  if (island_scenario == 41) {
    # Single lineage endemic (early col) & single lineage non-endemic (late col)
    island_spec <- data.frame(
      spec_id = c(2, 4),
      main_anc_id = c(2, 4),
      col_t = c(0.42, 0.67),
      spec_type = c("A", "I"),
      branch_code = c(as.character(NA),
                      as.character(NA)),
      branch_t = c(NaN, NaN),
      ana_origin = c("mainland_extinction",
                     as.character(NA)))
  }

  if (island_scenario == 42) {
    # Single lineage endemic (early col) & single lineage endemic (late col)
    island_spec <- data.frame(
      spec_id = c(2, 4),
      main_anc_id = c(2, 4),
      col_t = c(0.42, 0.67),
      spec_type = c("A", "A"),
      branch_code = c(as.character(NA),
                      as.character(NA)),
      branch_t = c(NaN, NaN),
      ana_origin = c("mainland_extinction",
                     "mainland_extinction"))
  }

  if (island_scenario == 43) {
    # Single lineage endemic (early col) & single lineage endemic (late col)
    island_spec <- data.frame(
      spec_id = c(2, 4),
      main_anc_id = c(2, 4),
      col_t = c(0.42, 0.67),
      spec_type = c("A", "A"),
      branch_code = c(as.character(NA),
                      as.character(NA)),
      branch_t = c(NaN, NaN),
      ana_origin = c("mainland_extinction",
                     "mainland_extinction"))
  }

  if (island_scenario == 44) {
    island_spec <- data.frame(
      spec_id = c(1, 4),
      main_anc_id = c(1, 3),
      col_t = c(0.16, 0.67),
      spec_type = c("A", "A"),
      branch_code = c(as.character(NA),
                      as.character(NA)),
      branch_t = c(NaN, NaN),
      ana_origin = c("mainland_extinction",
                     "immig_parent"))
  }

  if (island_scenario == 45) {
    island_spec <- data.frame(
      spec_id = c(4, 3, 5),
      main_anc_id = c(1, 1, 1),
      col_t = c(0.16, 0.16, 0.16),
      spec_type = c("C", "C", "C"),
      branch_code = c("AA", "B", "AB"),
      branch_t = c(0.16, 0.5, 0.9),
      ana_origin = c(as.character(NA),
                     as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 46) {
    island_spec <- data.frame(
      spec_id = 1,
      main_anc_id = 1,
      col_t = 0.5,
      spec_type = "I",
      branch_code = as.character(NA),
      branch_t = NaN,
      ana_origin = as.character(NA))
  }

  if (island_scenario == 47) {
    island_spec <- data.frame(
      spec_id = c(1, 2),
      main_anc_id = c(1, 2),
      col_t = c(0.16, 0.5),
      spec_type = c("I", "I"),
      branch_code = c(as.character(NA),
                      as.character(NA)),
      branch_t = c(NaN, NaN),
      ana_origin = c(as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 48) {
    island_spec <- data.frame(
      spec_id = 1,
      main_anc_id = 1,
      col_t_bp = 0.5,
      spec_type = "I",
      branch_code = as.character(NA),
      branch_t_bp = NaN,
      ana_origin = as.character(NA))
  }

  if (island_scenario == 49) {
    island_spec <- data.frame(
      spec_id = 2,
      main_anc_id = 1,
      col_t_bp = 0.5,
      spec_type = "A",
      branch_code = as.character(NA),
      branch_t_bp = NaN,
      ana_origin = "immig_parent")
  }

  if (island_scenario == 50) {
    island_spec <- data.frame(
      spec_id = c(2, 3),
      main_anc_id = c(1, 1),
      col_t_bp = c(0.5, 0.5),
      spec_type = c("C", "C"),
      branch_code = c("A", "B"),
      branch_t_bp = c(0.5, 0.25),
      ana_origin = c(as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 51) {
    island_spec <- data.frame(
      spec_id = c(2, 1),
      main_anc_id = c(1, 1),
      col_t_bp = c(0.5, 0.3),
      spec_type = c("A", "I"),
      branch_code = c(as.character(NA),
                      as.character(NA)),
      branch_t_bp = c(NaN, NaN),
      ana_origin = c("immig_parent",
                     as.character(NA)))
  }

  if (island_scenario == 52) {
    island_spec <- data.frame(
      spec_id = c(2, 3, 1),
      main_anc_id = c(1, 1, 1),
      col_t_bp = c(0.5, 0.5, 0.25),
      spec_type = c("C", "C", "I"),
      branch_code = c("A", "B", as.character(NA)),
      branch_t_bp = c(0.5, 0.4, 0.25),
      ana_origin = c(as.character(NA),
                     as.character(NA),
                     as.character(NA)))
  }

  if (island_scenario == 53) {
    island_spec <- data.frame(
      spec_id = c(2, 3),
      main_anc_id = c(1, 1),
      col_t_bp = c(0.5, 0.25),
      spec_type = c("A", "A"),
      branch_code = c(as.character(NA),
                      as.character(NA)),
      branch_t_bp = c(NaN, NaN),
      ana_origin = c("immig_parent",
                     "immig_parent"))
  }

  if (island_scenario == 54) {
    island_spec <- data.frame(
      spec_id = c(2, 3, 4),
      main_anc_id = c(1, 1, 1),
      col_t_bp = c(0.5, 0.5, 0.25),
      spec_type = c("C", "C", "A"),
      branch_code = c("A", "B", as.character(NA)),
      branch_t_bp = c(0.5, 0.4, 0.25),
      ana_origin = c(as.character(NA),
                     as.character(NA),
                     "immig_parent"))
  }

  return(island_spec)
}
