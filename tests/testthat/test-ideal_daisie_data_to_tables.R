test_that("use", {
  set.seed(
    4,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_data <- sim_island_with_mainland(
    total_time = 1.0,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1)
  expect_error(plot_island(daisie_data), "Argument 4 must have names")
  ideal_daisie_data_to_tables(ideal_daisie_data = daisie_data$ideal_islands)
})

test_that("search for non-dull scenario", {
  skip("All scenarios are dull")
  for (seed in seq_len(1000)) {
    message(seed)
    set.seed(
      seed,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
    total_time <- 1.0
    m <- 10
    mainland <- sim_mainland(
      total_time = total_time,
      m = m,
      mainland_ex = 2.0
    )
    mainland_clade_index <- sample(seq_len(length(mainland)), size = 1)
    message("mainland_clade_index: ", mainland_clade_index)
    mainland_clade <- mainland[[mainland_clade_index]]
    mainland_sample_prob <- sample(c(0.5, 0.9, 1.0), size = 1)
    message("mainland_sample_prob: ", mainland_sample_prob)
    mainland_sample_type <- sample(
      c("unsampled", "undiscovered", "complete"),
      size = 1
    )
    message("mainland_sample_type: ", mainland_sample_type)
    island <- sim_island(
      total_time = total_time,
      island_pars = c(1, 1, 10, 12, 1),
      mainland = mainland_clade,
      mainland_sample_prob = mainland_sample_prob,
      mainland_sample_type = mainland_sample_type)

    daisie_data <- format_to_daisie_data(
      island_replicates = island,
      total_time = total_time,
      m = m
    )
    ideal_daisie_data <- daisie_data$ideal_islands
    is_dull <- function(ideal_daisie_data) {
      length(ideal_daisie_data) == 2 &&
      length(ideal_daisie_data[[1]]) == 1 &&
      length(ideal_daisie_data[[1]][[1]]) == 2 &&
      ideal_daisie_data[[1]][[1]]$island_age == total_time &&
      ideal_daisie_data[[1]][[1]]$not_present == 0 &&
      length(ideal_daisie_data[[2]]) == 1 &&
      length(ideal_daisie_data[[2]][[1]]) == 2 &&
      ideal_daisie_data[[2]][[1]]$island_age == total_time &&
      ideal_daisie_data[[2]][[1]]$not_present == 0
    }
    expect_true(is_dull(daisie_data$ideal_islands))
    expect_true(is_dull(daisie_data$empirical_islands))
  }
})
