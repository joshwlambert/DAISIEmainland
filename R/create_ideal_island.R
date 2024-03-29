#' Convert intermediate output to final simulation output for ideal data
#'
#' @inheritParams default_params_doc
#'
#' @return a list with these elements:
#' \enumerate{
#'   \item `branching_times`, a sorted numeric vector, as required
#'     by the ML estimation functions. The first element always refers to
#'     the island age. Subsequent elements refer to colonisation, speciation and
#'     recolonisation times. The most recent recolonisation time, if any is
#'     always omitted to approximate simulation results to the mathematical
#'     formulation of the likelihood functions used for MLE.
#'   \item `stac`, status of colonist. In this function it can be
#'     returned as either 2, 4 or 3. If `stac` is 2, then there is only one
#'     independent colonisation present on the island and the extant species are
#'     endemic. If stac is 4, then only a singleton endemic is present at the
#'     present. If stac is 3, then recolonisation occurred, and more than one
#'     colonising lineage.
#'   \item `missing_species`, a numeric value with the number of
#'     missing species, that is, species not sampled in the phylogeny but
#'     present on the island. As this code only runs for simulation models,
#'     here `missing_species` is always set to 0.
#'   \item `all_colonisations`, on recolonising lineages only. It is
#'     comprised of `$event_times` and `$species_type`:
#'     \describe{
#'       \item{`$event_times`}{ordered numeric vectors containing all
#'       events for each extant recolonising lineage. This includes all
#'       colonisation and branching times. Each vector pertains to one
#'       colonising lineage.}
#'       \item{`$species_type`}{a string. Can be `"A"`, `"C"` or
#'       `"I"` depending on whether the extant clade is of anagenetic,
#'       cladogenetic or immigrant origin, respectively.}
#'   }
#' }
#' @keywords internal
#' @author Joshua W. Lambert
create_ideal_island <- function(total_time,
                                island_tbl) {
  ### number of independent colonisations
  uniquecolonisation <- as.numeric(unique(
    island_tbl[, "col_t_bp"]
  ))
  number_colonisations <- length(uniquecolonisation)
  ### if there is only one independent colonisation - anagenetic and
  ### cladogenetic species are classed as stac=2; immigrant classed as stac=4:
  if (number_colonisations == 1) {
    if (island_tbl[1, "spec_type"] == "I") {
      descendants <- list(
        branching_times = c(total_time, as.numeric(island_tbl[1, "col_t_bp"])),
        stac = 4,
        missing_species = 0
      )
    }
    if (island_tbl[1, "spec_type"] == "A") {
      descendants <- list(
        branching_times = c(total_time, as.numeric(island_tbl[1, "col_t_bp"])),
        stac = 2,
        missing_species = 0
      )
    }
    if (island_tbl[1, "spec_type"] == "C") {
      descendants <- list(
        branching_times = c(
          total_time,
          sort(as.numeric(island_tbl[, "branch_t_bp"]),
            decreasing = TRUE
          )
        ),
        stac = 2,
        missing_species = 0
      )
    }
  }

  ### if there are two or more independent colonisations, all species are
  ### classed as stac=3 and put within same list item:
  if (number_colonisations > 1) {
    descendants <- list(
      branching_times = NaN,
      stac = 3,
      missing_species = 0,
      all_colonisations = list()
    )

    # Get branching and colonisation times
    btimes_all_clado_desc <- rev(
      sort(as.numeric(island_tbl[, "branch_t_bp"]))
    )
    col_times <- sort(
      unique(as.numeric(island_tbl[, "col_t_bp"])),
      decreasing = TRUE
    )

    # If there are endemic descendants find youngest col time
    if (length(btimes_all_clado_desc) != 0) {
      # Ensure all col_times are in b_times at this point.
      # Covers cases of one recolonization followed by cladogenesis and
      # potential extinction
      if (any(!(col_times %in% btimes_all_clado_desc))) {
        miss_col_time <- which(!(col_times %in% btimes_all_clado_desc))
        btimes_all_clado_desc <- sort(
          c(btimes_all_clado_desc, col_times[miss_col_time]),
          decreasing = TRUE
        )
      }
      youngest_col_time <- min(col_times)
      i_youngest_col_btimes <- which(btimes_all_clado_desc == youngest_col_time)

      # Remove youngest col time in branching times
      testit::assert(youngest_col_time %in% btimes_all_clado_desc)
      btimes_all_clado_desc <- btimes_all_clado_desc[-i_youngest_col_btimes]

      descendants$branching_times <- c(total_time, btimes_all_clado_desc)
      testit::assert(!(youngest_col_time %in% btimes_all_clado_desc))

      # If no cladogenetic species is present, remove the youngest col time
    } else if (length(btimes_all_clado_desc) == 0) {
      youngest_col_time <- min(col_times)
      i_youngest_col_time <- which(col_times == youngest_col_time)
      col_times <- col_times[-i_youngest_col_time]

      descendants$branching_times <- c(total_time, col_times)
    }


    # all_colonisations section
    uniquecol <- sort(as.numeric(
      unique(island_tbl[, "col_t_bp"])
    ), decreasing = TRUE)
    for (i in seq_along(uniquecol)) {
      descendants$all_colonisations[[i]] <- list(
        event_times = NaN,
        species_type = as.character(NA)
      )

      samecolonisation <- which(as.numeric(
        island_tbl[, "col_t_bp"]
      ) == uniquecol[i])

      if (island_tbl[samecolonisation[1], "spec_type"] == "I") {
        descendants$all_colonisations[[i]]$event_times <- as.numeric(
          c(total_time, island_tbl[samecolonisation, "col_t_bp"])
        )
        descendants$all_colonisations[[i]]$species_type <- "I"
      }

      if (island_tbl[samecolonisation[1], "spec_type"] == "A") {
        descendants$all_colonisations[[i]]$event_times <- as.numeric(
          c(total_time, island_tbl[samecolonisation, "col_t_bp"])
        )
        descendants$all_colonisations[[i]]$species_type <- "A"
      }

      if (island_tbl[samecolonisation[1], "spec_type"] == "C") {
        descendants$all_colonisations[[i]]$event_times <-
          sort(c(total_time, as.numeric(
            island_tbl[samecolonisation, "branch_t_bp"]
          )), decreasing = TRUE)
        descendants$all_colonisations[[i]]$species_type <- "C"
      }
    }
  }
  return(descendants)
}
