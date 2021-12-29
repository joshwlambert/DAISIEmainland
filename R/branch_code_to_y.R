#' Convert a `branch_code` to a y coordinat in range `[0, 1]`
#'
#' Convert a `branch_code` to a y coordinat in range `[0, 1]`.
#'
#' The y coordinat is the result of a division:
#'   * numerator: for a binary value of `b` (converted from the `branch_code`,
#'     where the `A` equals a zero, and the `B` equals a
#'     one), the numerator in this division
#'     equals `1 + 2b`
#'   * denominator: for a `branch_code` of length `n`,
#'     the denominator equals `2 ^ n`.
#'
#' @inheritParams default_params_doc
#'
#' @return a y coordinat in range `[0, 1]`
#'
#' @examples
#' branch_code_to_y("A")
#' branch_code_to_y("AA")
#' branch_code_to_y("AB")
#' @author Richèl J.C. Bilderbeek
#' @export
branch_code_to_y <- function(branch_code) {
  DAISIEmainland::check_branch_code(branch_code)

  split_branch_code <- strsplit(branch_code, split = "")[[1]]
  split_branch_code[split_branch_code == "A"] <- 0
  split_branch_code[split_branch_code == "B"] <- 1
  binary_branch_code <- paste0(split_branch_code, collapse = "")
  branch_code_value <- strtoi(binary_branch_code, base = 2)
  numerator <- 1 + (2 * branch_code_value)
  denominator <- 2 ^ nchar(branch_code)
  numerator / denominator
}
