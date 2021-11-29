test_that("minimal example use", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland <- DAISIEmainland:::sim_mainland(
    total_time = 1,
    m = 2,
    mainland_ex = 1)

  plot_mainland(mainland)
})

test_that("interesting example", {
  set.seed(
    4,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland <- DAISIEmainland:::sim_mainland(
    total_time = 1,
    m = 4,
    mainland_ex = 1)

  plot_mainland(mainland)
})
