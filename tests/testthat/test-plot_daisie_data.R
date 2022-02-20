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

test_that("many clades", {
  set.seed(
    274,
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

test_that("Issue #68: plot all recolonisations", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )

  daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 50,
    island_pars = c(1.0, 0.5, 10, 0.1, 0.5),
    mainland_ex = 2,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  DAISIEmainland::plot_daisie_mainland_data(
    daisie_mainland_data = daisie_mainland_data,
    replicate_index = 1
  )
  # Plots nicely
  daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  ideal_daisie_data <- daisie_data
  plot_daisie_data(daisie_data)

  # Plots nicely when there are no colonisations
  daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[1]]
  plot_daisie_data(daisie_data)
})


test_that("Issue #68: plot all recolonisations with many branches", {
  set.seed(
    631,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )

  daisie_mainland_data <- DAISIEmainland::sim_island_with_mainland(
    total_time = 1,
    m = 10,
    island_pars = c(1, 1, 10, 0.1, 1),
    mainland_ex = 1,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete",
    replicates = 1,
    verbose = FALSE
  )
  DAISIEmainland::plot_daisie_mainland_data(
    daisie_mainland_data = daisie_mainland_data,
    replicate_index = 1
  )
  # Plots nicely
  daisie_data <- daisie_mainland_data$ideal_multi_daisie_data[[1]]
  plot_daisie_data(daisie_data)

  # Plots nicely when there are no colonisations
  daisie_data <- daisie_mainland_data$empirical_multi_daisie_data[[1]]
  plot_daisie_data(daisie_data)
})

test_that("Search for interesting scenarions", { # nolint indeed, this is complex :-)
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
    empirical_daisie_data <-
      daisie_mainland_data$empirical_multi_daisie_data[[1]]
    if ("stress-test" == "not now") {
      plot_daisie_data(daisie_data = ideal_daisie_data)
      plot_daisie_data(empirical_daisie_data)
    }
    if (length(ideal_daisie_data) == 1) next
    if ("look for recolonisations and branching" == "it") {
      for (clade_index in seq(2, length(ideal_daisie_data))) {
        n_colonisations <- length(ideal_daisie_data[[clade_index]]$all_colonisations) # nolint indeed a long line
        n_branches <- length(ideal_daisie_data[[clade_index]]$branching_times) - 1 # nolint indeed a long line
        if (n_colonisations > 1 && n_branches > n_colonisations) {
          message(seed)
          stop(seed)
        }
      }
    }
    if ("look for interesting stac" == "what I want") {
      uninteresting_set <- c(2, 4, 3, 5, 6)
      if (!ideal_daisie_data[[2]]$stac %in% uninteresting_set ||
          !empirical_daisie_data[[2]]$stac %in% uninteresting_set) {
        message(seed)
        stop(seed)
      }
    }
    if ("look for many branches" == "what I want") {
        if (length(ideal_daisie_data[[2]]$branching_times) > 3) {
        message(seed)
        stop(seed)
        }
    }
    if ("look for many clades" == "what I want") {
      if (length(ideal_daisie_data) > 3) {
        message(seed)
        stop(seed)
      }
    }
  }
})
