context("sample_mainland")

test_that("sample_mainland runs silent and produces correct output complete
          sampling", {
  # RJCB: I would love a 'create_test_mainland_1' function here!
  mainland <- rbind(c("2", "2", "0", "E", "A", NA, NA, "0",
                      "0.779042070209266"),
                    c("27", "2", "0", "C", "AA", "0.779042070209266", NA,
                      "0.779042070209266", "1"),
                    c("28", "2", "0", "C", "AB", "0.779042070209266", NA,
                      "0.779042070209266", "1"))

  # From both the documentation and this test, I still do not have
  # an idea what 'sample_mainland' does. I would naively think
  # if a 'mainland' is sampled, this happens at a certain time
  # (?totaltime) and thay mainland species IDs are returned.
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
  # RJCB: I would love a 'create_test_mainland_2' function here!
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
