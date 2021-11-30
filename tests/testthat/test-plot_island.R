test_that("use", {
  set.seed(
    1,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland <- DAISIEmainland:::sim_mainland(
    total_time = 1,
    m = 10,
    mainland_ex = 2)
  plot_mainland(mainland = mainland)

  count_extant_mainland_species <- function(mainland) {
    purrr::map_dbl(mainland, function(x) { return(sum(x$spec_ex_t == 1.0)) } )
  }
  clade_with_most_species <- which(
    count_extant_mainland_species(mainland) ==
      max(count_extant_mainland_species(mainland))
  )

  mainland_clade <- mainland[[clade_with_most_species]]
  plot_mainland_clade(mainland_clade) # Indeed an interesting mainland clade

  cladogenesis_rate <- 1.0
  extinction_rate <- 1.0
  carrying_capacity <- 10.0
  immigration_rate <- 1.0
  anagenesis_rate <- 1.0

  island_pars <- c(
    cladogenesis_rate,
    extinction_rate,
    carrying_capacity,
    immigration_rate,
    anagenesis_rate
  )
  mainland_sample_type <- "unsampled"
  mainland_sample_type <- "undiscovered"
  mainland_sample_type <- "complete"

  island <- sim_island(
    total_time = 1,
    island_pars = island_pars,
    mainland_clade = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = mainland_sample_type)
  island
  skip("TODO: add 'plot_island'")
  plot_island(island)
})

test_that("use", {
  set.seed(
    2,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  mainland_clade <- create_test_mainland_clade(mainland_scenario = 2)
  plot_mainland_clade(mainland_clade)
  island <- sim_island(
    total_time = 1,
    island_pars = c(1, 1, 10, 1, 1),
    mainland = mainland_clade,
    mainland_sample_prob = 1,
    mainland_sample_type = "complete")
  skip("TODO: add 'plot_island'")
  plot_ideal_island(island$ideal_island)
  plot_empirical_island(island$empirical_island)
  plot_island(island)
})
