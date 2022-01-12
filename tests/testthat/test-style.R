test_that("follow Tidyverse style", {
  skip_if(isTRUE(as.logical(Sys.getenv("CI"))))
  lintr::expect_lint_free()
})
