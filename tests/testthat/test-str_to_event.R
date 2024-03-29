test_that("use", {
  expect_equal(str_to_event("immigration"), 1)
  expect_equal(str_to_event("extinction"), 2)
  expect_equal(str_to_event("anagenesis"), 3)
  expect_equal(str_to_event("cladogenesis"), 4)
  expect_error(str_to_event(0))
  expect_error(str_to_event(5))
  expect_error(str_to_event(NA))
  expect_error(str_to_event(NULL))
  expect_error(str_to_event(Inf))
  expect_error(str_to_event(c(1, 2)))
  expect_error(str_to_event(c()))
  expect_error(str_to_event(list()))
  expect_error(str_to_event("nonsense"))
})

test_that("symmetry", {
  expect_equal("immigration", event_to_str(str_to_event("immigration")))
  expect_equal("extinction", event_to_str(str_to_event("extinction")))
  expect_equal("anagenesis", event_to_str(str_to_event("anagenesis")))
  expect_equal("cladogenesis", event_to_str(str_to_event("cladogenesis")))
})
