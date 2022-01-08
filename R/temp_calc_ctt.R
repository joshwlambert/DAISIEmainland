# # extract multi daisie data from daisie mainland data
# multi_daisie_data <- daisie_mainland_data$ideal_multi_daisie_data
# # extract daisie data from multi daisie data
# daisie_data <- multi_daisie_data[[i]]
# #remove daisie meta data
# daisie_data <- daisie_data[-1]
# # extract branching times
# branching_times <- lapply(daisie_data, "[[", "branching_times")
# stacs <- sapply(daisie_data, "[[", "stac")
# ideal_col_times <- lapply(daisie_data, function(x) {
#   stacs <- x$stac
#   if (stacs %in% c(1, 2, 4, 5, 6)) {
#     x$branching_times[2]
#   } else {
#     event_times <- lapply(x$all_colonisations, "[[", "event_times")
#     event_times[[2]]
#   }
# })
