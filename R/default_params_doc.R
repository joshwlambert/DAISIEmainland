#' Documentation for function arguments in the DAISIEmainland package
#'
#' @param branch_code a species' branching pattern,
#' as checked by \link{check_branch_code}, e.g. `A`, `AB`, `AABA`.
#' @param branching_times
#' island age and stem age of the
#' population/species in the case of Non-endemic, Non-endemic_MaxAge and
#' Endemic anagenetic species.
#'
#' For cladogenetic species these should
#' be island age and branching times of the radiation including the
#' stem age of the radiation.
#'
#' See \link{check_branching_times} for a more detailed description.
#' @param event_times the island age, stem age of species,
#' and -for cladogenetic species- speciation times,
#' as checked by \link{check_event_times}
#' @param missing_species the number of island species that were not
#' sampled for particular clade (only applicable for endemic clades),
#' as can be checked by \link{check_missing_species}
#' @param species_type the type of species:
#'
#'  * `"A"`: the extant clade is of anagenetic origin
#'  * `"C"`: the extant clade is of cladogenetic origin
#'  * `"I"`: the extant clade is of immigrant origin
#'
#' Use \link{check_species_type} to check if the `species_type` is valid
#' @param stac the status of the colonist,
#' see \link{stac_to_str} for values and meanings
#' @param timeval Numeric defining current time of simulation.
#' @param total_time Numeric defining the length of the simulation in time
#' units.
#' @param gam Numeric with the per capita immigration rate.
#' @param laa Numeric with the per capita anagenesis rate.
#' @param lac Numeric with the per capita cladogenesis rate.
#' @param mu Numeric with the per capita extinction rate.
#' @param k Numeric with carrying capacity.
#' @param num_spec Numeric with the current number of species.
#' @param num_immigrants Numeric with the current number of non-endemic
#' species.
#' @param mainland_n Numeric stating the number of mainland species that
#' can colonize the island.
#' @param island_tbl Data frame with current state of simulation containing
#' number of species, see [create_test_island_tbl()]
#' @param multi_mainland_clade A list of data frames each data frame is an
#' `mainland_clade`
#' @param mainland_clade Data frame with state of mainland, see
#' [create_test_mainland_clade()]
#' @param mainland_spec Numeric focal species on the mainland
#' @param mainland_sample_prob Numeric between zero and one determining the
#' probability of an extant mainland species being sampled.
#' @param mainland_sample_type String either "unsampled" for unsampled known
#' species on the mainland, "undiscovered" for undiscovered species on the
#' mainland, or "complete" if the mainland sampling probability is one.
#' @param m Numeric defining the size of mainland pool.
#' @param verbose Logical, determining if progress output should be printed
#' during the simulation.
#' @param rates named list of numeric rates as returned by \link{calc_rates}.
#' @param island_pars A numeric vector containing the parameters for the island:
#'   \itemize{
#'     \item{`island_pars[1]`: lambda^c (cladogenesis rate)}
#'     \item{`island_pars[2]`: mu (extinction rate)}
#'     \item{`island_pars[3]`: K (carrying capacity), set K=Inf for
#'     diversity independence.}
#'     \item{`island_pars[4]`: gamma (immigration rate)}
#'     \item{`island_pars[5]`: lambda^a (anagenesis rate)}
#'     }
#' @param mainland_ex Numeric parameter for mainland extinction rate.
#' @param replicates Integer specifying number of island replicates to be
#' simulated.
#' @param possible_event Numeric defining what event will happen.
#' @param max_spec_id Current species IDs.
#' @param mainland_scenario Integer determining which mainland clade scenario
#' is produced
#' @param anc_branch_t_bp Numeric ancestral branching time before the present
#' of the immigrant species ancestor and its extant relative on the mainland
#' @param subset_island Data frame of island species that originated from a
#' single colonisation event
#' @param daisie_mainland_data \link{list} containing data of a DAISIE
#' simulation with mainland dynamics,
#' as produced by \link{sim_island_with_mainland}
#' and checked by \link{check_daisie_mainland_data}
#'
#' A `daisie_mainland_data` has two elements:
#'  * `ideal_multi_daisie_data`: the ideal data set produced in a simulation,
#'    see \link{check_multi_daisie_data} for more details
#'  * `empirical_multi_daisie_data`: the empirical data sets produced in a
#'  simulation see \link{check_multi_daisie_data} for more details
#' @param daisie_data a \link[DAISIE]{DAISIE} `datalist`, as can be
#' checked by \link{check_daisie_data}. To quote the DAISE doc:
#'
#' A `datalist` is an object containing information on colonisation and
#' branching times. This object can be generated using
#' \link[DAISIE]{DAISIE_dataprep},
#' which converts a user-specified data table into a `datalist`,
#' but the object can of course also be entered directly.
#'
#' A `datalist` is a \link{list} with the following elements:
#'
#' The first element of the list has two or three components:
#'
#'  * \code{$island_age} - the island age
#'
#' Then, depending on whether a distinction between types is
#' made, we have:
#'
#'  * \code{$not_present} - the number of mainland lineages
#'   that are not present on the island \cr
#'
#'  or:
#'
#'   * \code{$not_present_type1} - the number of mainland lineages
#'     of type 1 that are not present on the island
#'   * \code{$not_present_type2} - the number of mainland lineages of
#'     type 2 that are not present on the island
#'
#' The remaining elements of
#' the `datalist` each contains information on a single colonist lineage on the
#'   island and has 5 components:
#'
#'    * \code{$colonist_name} - the name of the species or clade that
#'      colonized the island
#'    * \code{$branching_times} - island age followed by stem age of the
#'      population/species in the case of Non-endemic, Non-endemic_MaxAge
#'      species and Endemic species with no close relatives
#'      on the island. For endemic clades with more than one species on the
#'      island (cladogenetic clades/ radiations) these should be island age
#'      followed by the branching times of the island clade including the stem
#'      age of the clade
#'    * \code{$stac} - the status of the colonist, see \link{stac_to_str}
#'      for values and descriptions
#'    * \code{$missing_species} - number of island species that were not
#'      sampled for particular clade (only applicable for endemic clades)
#'    * \code{$type1or2} - whether the colonist belongs to type 1 or type 2
#' @param ideal_daisie_data a \link{list},
#' of which each element contains the history
#' of a DAISIE simulation with mainland dynamics, with a length that
#' equals the number of replicates.
#'
#' The simulated history is recorded perfectly, resulting in ideal data.
#' The list elements are of type `DAISIE::datalist`,
#' as checked by \link{check_daisie_data}
#' and can be used by \link[DAISIE]{DAISIE} (e.g. \link[DAISIE]{DAISIE_ML_CS})
#' @param empirical_daisie_data a list, of which each element contains the
#' history of a DAISIE simulation with mainland dynamics, with a length that
#' equals the number of replicates
#'
#' The simulated history is recorded as an empiricist would,
#' resulting in imperfect data.
#' The list elements are of type `DAISIE::datalist`,
#' as checked by \link{check_daisie_data}
#' and can be used by \link[DAISIE]{DAISIE} (e.g. \link[DAISIE]{DAISIE_ML_CS})
#' @param ideal_or_empirical_island the evolutionary history of
#' an island from ideal or empirical data,
#' as created by \link{sim_island}
#'
#' `ideal_or_empirical_island` is a list  containing 3 components:
#' * `$branching_times`: island age, species stem age and speciation times.
#'    See \link{check_branching_times} for more details
#' * `$stac`: the status of the colonist,
#'    see \link{stac_to_str} for values and meanings
#' * `$missing_species`: number of island species that were
#' not sampled for particular clade (only applicable for endemic clades)
#'
#' For recolonising lineages, there is an extra element,
#' `all_colonisations` per list element.
#' It is comprised of `$event_times` and `$species_type`:
#' \describe{
#'   \item{`$event_times`}{ordered numeric vectors containing all
#'     events for each extant recolonising lineage. This includes all
#'     colonisation and branching times. Each vector pertains to one
#'     colonising lineage.}
#'   \item{`$species_type`}{a string. Can be `"A"`, `"C"` or
#'     `"I"` depending on whether the extant clade is of anagenetic,
#'     cladogenetic or immigrant origin, respectively.}
#' }
#' @param species_type a string. Can be `"A"`, `"C"` or
#' `"I"` depending on whether the extant clade is of anagenetic,
#' cladogenetic or immigrant origin, respectively.
#' @param ideal_ml List containing maximum likelihood estimates from DAISIE
#' fitted to ideal data produced from `sim_island_with_mainland`. Output
#' from `DAISIE::DAISIE_ML_CS`
#' @param empirical_ml List containing maximum likelihood estimates from DAISIE
#' fitted to empirical data produced from `sim_island_with_mainland`.
#' Output from `DAISIE::DAISIE_ML_CS`
#' @param sim_params Vector of five numerics for the values of cladogenesis,
#' extinction, carrying capacity, immigration and anagenesis used to simulate
#' data
#' @param param_set Numeric for the index of the parameter set from the
#' parameter space
#' @param data_folder_path String specifying the directory the data is read
#' from
#' @param output_file_path String specifying the directory the file is saved
#' in, if NULL the file is returned to console and not saved
#' @param param_space String specifying which parameter space results to read.
#' Either "general", "mainland_ex", or "mainland_sample_prob"
#' @param parameter String specifying which parameter is plotted.
#' "mainland_ex" plots mainland extinction as the variable,
#' "mainland_sample_prob" plots mainland sampling probability as the variable
#' and "both" plots all the data
#' @param plotting_data Numeric vector of data points
#' @param breaks A vector of numerics
#' @param num_breaks A numeric specifying how many breaks are wanted on the
#' plot axes
#' @param signif A numeric specifying how many significant figures the axes
#' labels have when plotting
#' @param scientific A boolean determining whether the axis labels will be
#' converted to scientific notation
#' @param transform Either `NULL` or `"ihs"` to specify no transformation of
#' the data or an inverse hyperbolic sine (ihs) transformation
#' @param mainland the evolutionary history of the mainland species,
#' as created by \link{sim_mainland}.
#' Use \link{plot_mainland} to visualise the that evolutionary history.
#' @param age Numeric defining age of singleton species
#' @param multi_daisie_data A list of `daisie_data` elements
#' @param analysis_result A list containing ouput from the run_analysis script.
#' The list can be checked with `check_analysis_result`
#' @param analysis_results A list of `analysis_result` elements which can be
#' checked with `check_analysis_result`
#' @param labels A string or vector of strings to label the plotting grid
#' @param branch_colour A string, either "unique_species_id" or "clade_id" to
#' colour the branches of the mainland tree(s) with one of the attributes.
#'
#' @return Nothing
#' @author Joshua W. Lambert
default_params_doc <- function(branch_code,
                               branching_times,
                               event_times,
                               missing_species,
                               stac,
                               timeval,
                               total_time,
                               gam,
                               laa,
                               lac,
                               mu,
                               k,
                               num_spec,
                               num_immigrants,
                               mainland_n,
                               island_tbl,
                               multi_mainland_clade,
                               mainland_clade,
                               mainland_spec,
                               mainland_sample_prob,
                               mainland_sample_type,
                               m,
                               verbose,
                               rates,
                               island_pars,
                               mainland_ex,
                               replicates,
                               possible_event,
                               max_spec_id,
                               mainland_scenario,
                               anc_branch_t_bp,
                               subset_island,
                               daisie_mainland_data,
                               daisie_data,
                               ideal_daisie_data,
                               empirical_daisie_data,
                               ideal_or_empirical_island,
                               species_type,
                               ideal_ml,
                               empirical_ml,
                               sim_params,
                               param_set,
                               data_folder_path,
                               output_file_path,
                               param_space,
                               parameter,
                               plotting_data,
                               breaks,
                               num_breaks,
                               signif,
                               scientific,
                               transform,
                               mainland,
                               age,
                               multi_daisie_data,
                               analysis_result,
                               analysis_results,
                               labels,
                               branch_colour) {
  # Nothing
}
