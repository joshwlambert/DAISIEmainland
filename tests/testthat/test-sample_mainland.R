context("sample_mainland")

test_that("sample_mainland runs silent and produces correct output complete
          sampling", {
  mainland <- rbind(c("2", "2", "0", "E", "A", NA, NA, "0",
                      "0.779042070209266"),
                    c("27", "2", "0", "C", "AA", "0.779042070209266", NA,
                      "0.779042070209266", "1"),
                    c("28", "2", "0", "C", "AB", "0.779042070209266", NA,
                      "0.779042070209266", "1"))

  expect_silent(sampled_mainland <- sample_mainland(
    totaltime = 1,
    mainland = mainland,
    mainland_sample_prob = 1)
  )
  expect_equal(mainland, sampled_mainland)
})


test_that("sample_mainland runs silent and produces correct output incomplete
          sampling", {
  set.seed(1)
  mainland <- rbind(c("2", "2", "0", "E", "A", NA, NA, "0",
                      "0.779042070209266"),
                    c("27", "2", "0", "C", "AA", "0.779042070209266", NA,
                      "0.779042070209266", "1"),
                    c("28", "2", "0", "C", "AB", "0.779042070209266", NA,
                      "0.779042070209266", "1"))

  expect_silent(sampled_mainland <- sample_mainland(
    totaltime = 1,
    mainland = mainland,
    mainland_sample_prob = 0.5)
  )
  new_mainland <- rbind(c("2", "2", "0", "E", "A", NA, NA, "0",
                          "0.779042070209266"),
                        c("27", "2", "0", "E", "AA", "0.779042070209266", NA,
                          "0.779042070209266", "0.99999"),
                        c("28", "2", "0", "E", "AB", "0.779042070209266", NA,
                          "0.779042070209266", "0.99999"))
  expect_equal(new_mainland, sampled_mainland)
})
