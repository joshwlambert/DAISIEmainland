test_that("No extant colonists", {
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 10,
    verbose = FALSE
  )
  ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  empirical_daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[1]]
  daisie_data <- ideal_daisie_data
  expect_silent(plot_daisie_data(ideal_daisie_data))
  expect_silent(plot_daisie_data(empirical_daisie_data))
})

test_that("One colonist clade, stac = 4: Non_endemic", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "undiscovered",
    replicates = 1,
    verbose = FALSE
  )
  ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  empirical_daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[1]]
  daisie_data <- ideal_daisie_data
  plot_daisie_data(ideal_daisie_data)
  plot_daisie_data(empirical_daisie_data)
})

test_that("stac == 2: Endemic (and Non_endemic)", {
  set.seed(
    4,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  empirical_daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[1]]
  testthat::expect_true(
    ideal_daisie_data[[2]]$stac != 4 || empirical_daisie_data[[2]]$stac != 4
  )
  plot_daisie_data(daisie_data = ideal_daisie_data)
  plot_daisie_data(empirical_daisie_data)
})

test_that("stac == 5: Endemic_singleton_MaxAge in empirical_daisie_data", {
  set.seed(
    7,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  empirical_daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[1]]
  plot_daisie_data(daisie_data = ideal_daisie_data)
  plot_daisie_data(empirical_daisie_data)
})

test_that("stace == 6: Endemic_clade_MaxAge, in empirical_daisie_data", {
  set.seed(
    40,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  empirical_daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[1]]
  plot_daisie_data(daisie_data = ideal_daisie_data)
  plot_daisie_data(empirical_daisie_data)
})

test_that("stac == 3: Endemic&Non_Endemic", {
  set.seed(
    179,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  empirical_daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[1]]
  plot_daisie_data(daisie_data = ideal_daisie_data)
  plot_daisie_data(empirical_daisie_data)
})


test_that("much branching", {
  set.seed(
    1018,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  daisie_mainland_data <- sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  empirical_daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[1]]
  plot_daisie_data(daisie_data = ideal_daisie_data)
  plot_daisie_data(empirical_daisie_data)
})


test_that("Search for interesting scenarions", {
  skip("Only run locally")
  seed <- 0
  while (1) {
    seed <- seed + 1
    message(seed)
    set.seed(
      seed,
      kind = "Mersenne-Twister",
      normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
    daisie_mainland_data <- sim_island_with_mainland(
      total_time = 1,
      m = 10,
      island_pars = c(1, 1, 10, 0.1, 1),
      mainland_ex = 1,
      mainland_sample_prob = 1,
      mainland_sample_type = "complete",
      replicates = 1,
      verbose = FALSE
    )
    ideal_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
    empirical_daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[1]]
    if (length(ideal_daisie_data) == 1) next
    if ("look for interesting stac" == "what I want") {
      uninteresting_set <- c(2, 4, 3, 5, 6)
      if (!ideal_daisie_data[[2]]$stac %in% uninteresting_set || !empirical_daisie_data[[2]]$stac %in% uninteresting_set) {
        message(seed)
        stop(seed)
      }
    }
    if (length(ideal_daisie_data[[2]]$branching_times) > 3) {
      message(seed)
      stop(seed)
    }
  }
})
