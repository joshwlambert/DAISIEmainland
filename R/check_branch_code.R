#' Check if a `branch_code` is valid.
#' Will \link{stop} if not
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

  invisible(branch_code)
}
