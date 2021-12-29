#' Check if a `branch_code` is valid.
#'
#' Check if a `branch_code` is valid.
#' Will \link{stop} if not
#'
#' A valid `branch_code` ...
#'
#'  * is a string, i.e. a character vector with one element
#'  * consists out of characters that are either `A` or `B`
#'  * starts with `A`
#'
#' @inheritParams default_params_doc
#'
#' @return nothing
#'
#' @examples
#' check_branch_code("A")
#' check_branch_code("AA")
#' check_branch_code("AB")
#' @author Richèl J.C. Bilderbeek
#' @export
check_branch_code <- function(branch_code) {
  testthat::expect_equal(length(branch_code), 1)
  testthat::expect_true(is.character(branch_code))
  testthat::expect_true(nchar(branch_code) > 0)
  chars <- strsplit(branch_code, split = "")[[1]]
  testthat::expect_equal(chars[1], "A")
  testthat::expect_true(all(chars %in% c("A","B")))

  # As function is mainly called for side effect, see
  # See https://style.tidyverse.org/functions.html#return
  invisible(branch_code)
}
