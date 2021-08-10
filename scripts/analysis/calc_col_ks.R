result_files <- list.files(
  path = "results",
  pattern = "param_set",
  full.names = TRUE
)

for (i in seq_along(result_files)) {
  results <- readRDS(result_files[i])


  for (j in seq_along(results$island$ideal_islands)) {


    for (k in 2:length(results$island$ideal_islands[[j]])) {

    }
  }



  for (j in seq_along(results$island$empirical)) {
    for (k in 2:length(results$island$empirical_islands[[j]])) {

    }
  }


}
