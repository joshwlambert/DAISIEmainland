# DAISIEmainland data standards

## Reasoning

There are several data types used in this package. Some of them are novel to
this package and are not used elsewhere, others derive from other R packages.
This document provides a standard of how the data types are formatted and which
functions allow for checking the types.

## Data structure types

### `daisie_mainland_data`

* A list of 2 elements, `$ideal_daisie_data` and `$empirical_daisie_data`.
Each of these elements is in the format `daisie_data` (see below).

Function to check if it is a daisie_mainland_data is `is_daisie_mainland_data`

### `multi_daisie_mainland_data`

* A list of `daisie_mainland_data` objects.

Function to check if it is a multi_daisie_mainland is
`is_multi_daisie_mainland_data`

### `daisie_data`

* A list of n elements. The first element is a list containing the `island_age`
with a single numeric, and the number of species `not_present` on the island.
This first element in the `daisie_data` list can optionally include an
`stt_table`. The `stt_table` is a matrix with five columns and a variable number
of rows. Every subsequent element in the `daisie_data` list is data on an island
clade, containing: `branching_times`, `stac`, `missing_species`, and optionally
information on recolonisations `all_colonisations`. `branching_times` is a
numeric vector containing the island age, time of island colonisation, and an
subsequent branching times (all given in time before the present).

### `mainland_clade`

* A data frame of seven columns and one or more rows. The column names are:
  `spec_id`, `main_anc_id`, `spec_type`, `branch_code`, `branch_t`,
`spec_origin_t`, `spec_ex_t`.

### `multi_mainland_clade`

* A list of `mainland_clade` objects.


