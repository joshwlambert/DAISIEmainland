#' Convert a `branch_code` to a y coordinat in range [0, 1]
#'
#' A valid `branch_code` ...
#'
#'  * is a string, i.e. a character vector with one element
#'  * consists out of characters that are either `A` or `B`
#'  * starts with `A`
#'
#' @inheritParams default_params_doc
#'
#' @return a y coordinat in range [0, 1]
#'
#' @examples
#' branch_code_to_y("A")
#' branch_code_to_y("AA")
#' branch_code_to_y("AB")
#' @author Richèl J.C. Bilderbeek
#' @export
branch_code_to_y <- function(branch_code) {
  DAISIEmainland::check_branch_code(branch_code)
}
