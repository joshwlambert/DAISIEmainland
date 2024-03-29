test_that("use", {
  expect_equal(event_to_str(1), "immigration")
  expect_equal(event_to_str(2), "extinction")
  expect_equal(event_to_str(3), "anagenesis")
  expect_equal(event_to_str(4), "cladogenesis")
  expect_error(event_to_str(0))
  expect_error(event_to_str(5))
  expect_error(event_to_str(NA))
  expect_error(event_to_str(NULL))
  expect_error(event_to_str(Inf))
  expect_error(event_to_str(c(1, 2)))
  expect_error(event_to_str(c()))
  expect_error(event_to_str(list()))
  expect_error(event_to_str("nonsense"))
})

test_that("symmetry", {
  expect_equal(1, str_to_event(event_to_str(1)))
  expect_equal(2, str_to_event(event_to_str(2)))
  expect_equal(3, str_to_event(event_to_str(3)))
  expect_equal(4, str_to_event(event_to_str(4)))
})
