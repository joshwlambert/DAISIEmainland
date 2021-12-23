#' Documentation for function arguments in the DAISIEmainland package
#'
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
#' @param island_spec Matrix with current state of simulation containing number
#' of species, see [create_test_island_spec()]
#' @param mainland_clade Data frame with state of mainland, see
#' [create_test_mainland_clade()]
#' @param mainland_spec Numeric focal species on the mainland
#' @param mainland_sample_prob Numeric between zero and one determining the
#' probability of an extant mainland species being sampled.
#' @param mainland_sample_type String either "unsampled" for unsampled known
#' species on the mainland, "undiscovered" for undiscovered species on the
#' mainland, or "complete" if the mainland sampling probability is one.
#' @param island_replicates List that has as many elements as replicates. Each
#' element must be a list with the elements `island_age` and
#' `not_present`. ##### LOOK INTO THIS
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
#' @param daisie_datalist a \link[DAISIE]{DAISIE} `datalist`, as can be
#' checked by \link{check_daisie_datalist}. To quote the DAISE doc:
#'
#' Data object containing information on colonisation and
#'   branching times. This object can be generated using the
#'   \link[DAISIE]{DAISIE_dataprep}
#'   function, which converts a user-specified data table into a data object,
#'   but the object can of course also be entered directly.
#'   It is an R list object with the following elements.\cr The first element
#'   of the list has two or three components: \cr \cr \code{$island_age} - the
#'   island age \cr Then, depending on whether a distinction between types is
#'   made, we have:\cr \code{$not_present} - the number of mainland lineages
#'   that are not present on the island \cr or:\cr \code{$not_present_type1} -
#'   the number of mainland lineages of type 1 that are not present on the
#'   island \cr \code{$not_present_type2} - the number of mainland lineages of
#'   type 2 that are not present on the island \cr \cr The remaining elements of
#'   the list each contains information on a single colonist lineage on the
#'   island and has 5 components:\cr \cr \code{$colonist_name} - the name of the
#'   species or clade that colonized the island \cr \code{$branching_times} -
#'   island age followed by stem age of the population/species in the case of
#'   Non-endemic, Non-endemic_MaxAge species and Endemic species with no close relatives
#'   on the island. For endemic clades with more than one species on the island
#'   (cladogenetic clades/ radiations) these should be island age followed by the
#'   branching times of the island clade
#'   including the stem age of the clade\cr \code{$stac} - the
#'   status of the colonist \cr \cr * Non_endemic_MaxAge: 1 \cr * Endemic: 2
#'   \cr * Endemic&Non_Endemic: 3 \cr * Non_Endemic: 4 \cr
#'   * Endemic_Singleton_MaxAge: 5 \cr * Endemic_Clade_MaxAge: 6
#'   \cr * Endemic&Non_Endemic_Clade_MaxAge: 7 \cr
#'   \cr \code{$missing_species} - number of island species that were not
#'   sampled for particular clade (only applicable for endemic clades) \cr
#'   \code{$type1or2} - whether the colonist belongs to type 1 or type 2 \cr
#' @param daisie_data List containing data of DAISIE simulation with mainland
#' dynamics. Output from `sim_island_with_mainland`.
#'
#' A list. The highest level of the list has two elements called
#' `ideal_islands` and `empirical_islands` which corresponds to
#' the ideal and empirical data sets produce in each simulation. Within each
#' `ideal_islands` and `empirical_islands` each element is an
#' individual replicate. The first element of each replicate is composed of
#' island information containing:
#' \describe{
#'   \item{`$island_age`}{A numeric with the island age.}
#'   \item{`$not_present`}{the number of mainland lineages that are not
#'     present on the island.}
#' }
#' The subsequent elements of the list pertaining to each replicate contain
#' information on a single colonist lineage on the island and have 3 components:
#' \describe{
#'   \item{`$branching_times`}{island age and stem age of the
#'     population/species in the case of Non-endemic, Non-endemic_MaxAge and
#'     Endemic anagenetic species.
#'     For cladogenetic species these should
#'     be island age and branching times of the radiation including the
#'     stem age of the radiation.}
#'   \item{`$stac`}{An integer ranging from 1 to 6
#'   indicating the status of the colonist:
#'   \enumerate{
#'     \item Non_endemic_MaxAge
#'     \item Endemic
#'     \item Endemic&Non_Endemic
#'     \item Non_endemic_MaxAge
#'     \item Endemic_singleton_MaxAge
#'     \item Endemic_clade_MaxAge
#' }}
#' \item{`missing_species`}{number of island species that were
#' not sampled for particular clade (only applicable for endemic clades)}
#' }
#'
#' @param ideal_daisie_data a list, of which each element contains the history
#' of a DAISIE simulation with mainland dynamics, with a length that
#' equals the number of replicates.
#' The simulated history is recorded perfectly, resulting in ideal data.
#' The list elements are of type `DAISIE::datalist`,
#' as checked by \link{check_daisie_datalist}
#' and can be used by \link[DAISIE]{DAISIE} (e.g. \link[DAISIE]{DAISIE_ML_CS})
#' @param empirical_daisie_data a list, of which each element contains the
#' history of a DAISIE simulation with mainland dynamics, with a length that
#' equals the number of replicates.
#' The simulated history is recorded as an empiricist would,
#' resulting in imperfect data.
#' The list elements are of type `DAISIE::datalist`,
#' as checked by \link{check_daisie_datalist}
#' and can be used by \link[DAISIE]{DAISIE} (e.g. \link[DAISIE]{DAISIE_ML_CS})
#' @param ideal_island a simulated island with perfect information,
#' as created by \link{sim_island} (together with `empirical_island`,
#' which has imperfect information).
#'
#' `ideal_island` is a list  containing 3 components:
#' * `$branching_times`: island age and stem age of the
#'    population/species in the case of Non-endemic, Non-endemic_MaxAge and
#'    Endemic anagenetic species.
#'
#'    For cladogenetic species these should
#'    be island age and branching times of the radiation including the
#'    stem age of the radiation.
#' * `$stac`: An integer ranging from 1 to 6
#'    indicating the status of the colonist:
#'    1. Non_endemic_MaxAge
#'    2. Endemic
#'    3. Endemic&Non_Endemic
#'    4. Non_endemic
#'    5. Endemic_singleton_MaxAge
#'    6. Endemic_clade_MaxAge
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
#' @param empirical_island a simulated island with imperfect information,
#' as created by \link{sim_island} (together with `ideal_island`,
#' which has perfect information).
#'
#' `empirical_island` is a list  containing 3 components:
#' * `$branching_times`: island age and stem age of the
#'    population/species in the case of Non-endemic, Non-endemic_MaxAge and
#'    Endemic anagenetic species.
#'
#'    For cladogenetic species these should
#'    be island age and branching times of the radiation including the
#'    stem age of the radiation.
#' * `$stac`: An integer ranging from 1 to 6
#'    indicating the status of the colonist:
#'    1. Non_endemic_MaxAge
#'    2. Endemic
#'    3. Endemic&Non_Endemic
#'    4. Non_endemic
#'    5. Endemic_singleton_MaxAge
#'    6. Endemic_clade_MaxAge
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
#' @param ideal_or_empirical_island the evolutionary history of
#' an island from ideal or empirical data,
#' as created by \link{sim_island}
#'
#' `ideal_or_empirical_island` is a list  containing 3 components:
#' * `$branching_times`: island age and stem age of the
#'    population/species in the case of Non-endemic, Non-endemic_MaxAge and
#'    Endemic anagenetic species.
#'
#'    For cladogenetic species these should
#'    be island age and branching times of the radiation including the
#'    stem age of the radiation.
#' * `$stac`: An integer ranging from 1 to 6
#'    indicating the status of the colonist:
#'    1. Non_endemic_MaxAge
#'    2. Endemic
#'    3. Endemic&Non_Endemic
#'    4. Non_endemic
#'    5. Endemic_singleton_MaxAge
#'    6. Endemic_clade_MaxAge
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
#' @param mainland the evolutionary history of the mainland species,
#' as created by \link{sim_mainland}.
#' Use \link{plot_mainland} to visualise the that evolutionary history.
#' @param island The history of a a single island.
#' A \link{list} with two elements:
#'
#'  * `ideal_island`: the island history based on ideal data
#'  * `empirical_island`: the island history based on empirical data
#'
#' Each of these is a \link{list} with the following elements:
#' * `$branching_times`: island age and stem age of the
#'    population/species in the case of Non-endemic, Non-endemic_MaxAge and
#'    Endemic anagenetic species.
#'
#'    For cladogenetic species these should
#'    be island age and branching times of the radiation including the
#'    stem age of the radiation.
#' * `$stac`: An integer ranging from 1 to 6
#'    indicating the status of the colonist:
#'    1. Non_endemic_MaxAge
#'    2. Endemic
#'    3. Endemic&Non_Endemic
#'    4. Non_endemic
#'    5. Endemic_singleton_MaxAge
#'    6. Endemic_clade_MaxAge
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
#' @param lower_lim Numeric stating the lower bound of the axis to be plotted
#' @param upper_lim Numeric stating the upper bound of the axis to be plotted
#' @param accuracy Numeric stating to which decimal place to kept, e.g. 0.1 is
#' to one decimal place
#' @param round_func Function to be passed through to the rounding, either
#' `floor` or `ceiling` function
#'
#' @return Nothing
#' @author Joshua W. Lambert
default_params_doc <- function(timeval,
                               total_time,
                               gam,
                               laa,
                               lac,
                               mu,
                               k,
                               num_spec,
                               num_immigrants,
                               mainland_n,
                               island_spec,
                               mainland_clade,
                               mainland_spec,
                               mainland_sample_prob,
                               mainland_sample_type,
                               island_replicates,
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
                               daisie_datalist,
                               daisie_data,
                               ideal_daisie_data,
                               empirical_daisie_data,
                               ideal_island,
                               empirical_island,
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
                               mainland,
                               island,
                               lower_lim,
                               upper_lim,
                               accuracy,
                               round_func) {
  #Nothing
}
