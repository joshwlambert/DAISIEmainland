#' Calculates the 5th, 25th, 50th, 75th and 95th percentile of the data
#'
#' @inheritParams default_params_doc
#'
#' @return Named vector of 5 named elements
calc_quantiles <- function(plotting_data) {
  quantiles <- stats::quantile(plotting_data,
                               probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(quantiles) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(quantiles)
}

#' Subsets the data to those less than the 5th percentile and those greater than
#' the 95th percentile
#'
#' @inheritParams default_params_doc
#'
#' @return Numeric vector
calc_outliers <- function(plotting_data) {
  outliers <- subset(plotting_data,
                     plotting_data < stats::quantile(plotting_data,
                                                     prob = 0.05) |
                       plotting_data > stats::quantile(plotting_data,
                                                       prob = 0.95))
  if (length(outliers) == 0) {
    outliers <- stats::median(plotting_data)
  }
  return(outliers)
}

#' Creates labels for plots by calling `scientific`.
#'
#' @inheritParams default_params_doc
#'
#' @return A function that takes a vector
create_labels <- function(signif) {
  function(breaks) scientific(
    breaks,
    signif = signif
  )
}

#' Creates the axis numbers (labels) for plotting with scientific form in
#' x10 notation
#'
#' @inheritParams default_params_doc
#'
#' @return Character vector
scientific <- function(breaks, signif) {
  breaks <- gsub(pattern = "e\\+",
                 replacement = "%*%10^",
                 x = choose_scientific(breaks, signif))
  parse(text = gsub(pattern = "e",
                    replacement = "%*%10^",
                    x = breaks))
}

#' Decides whether number should be in normal or scientific form depending on
#' the magnitude of the number
#'
#' @inheritParams default_params_doc
#'
#' @return Character vector
choose_scientific <- function(breaks, signif) {
  ifelse(breaks > 1e3 | breaks < 1e-3,
         scales::scientific(breaks, digits = 1),
         scales::number(signif(breaks, digits = signif), big.mark = ""))
}

#' Convert an `species_type` to a string,
#' where the `species_type` denotes the origin of a species.
#'
#' @inheritParams default_params_doc
#'
#' @return the species type as a string
#'
#' @examples
#' species_type_to_str("A") # Anagenetic
#' species_type_to_str("C") # Cladogentic
#' species_type_to_str("I") # Immigrant
#'
#' @author Richèl J.C. Bilderbeek
#'
#' @export
species_type_to_str <- function(species_type) {
  testthat::expect_equal(length(species_type), 1)
  if (species_type == "A") return("anagenetic")
  if (species_type == "C") return("cladogenetic")
  testthat::expect_equal(species_type, "I")
  return("immigrant")
}

#' Creates the axis numbers (breaks) for plotting with an inverse hyperbolic
#' sine transformation, with rounding to a set accuracy to reduce decimal
#' places plotted
#'
#' @inheritParams default_params_doc
#'
#' @return Numeric vector
create_plot_breaks <- function(lower_lim, upper_lim, accuracy, round_func) {
  breaks <- sinh(labeling::extended(asinh(lower_lim),
                                    asinh(upper_lim),
                                    m = 4))
  breaks <- round_func(breaks/accuracy) * accuracy
  return(breaks)
}

#' Creates the axis numbers (labels) for plotting with an inverse hyperbolic
#' sine transformation, with rounding to a set accuracy to reduce decimal
#' places plotted
#'
#' @inheritParams default_params_doc
#'
#' @return Character vector
create_plot_labels <- function(lower_lim, upper_lim, accuracy, round_func) {
  breaks <- sinh(labeling::extended(asinh(lower_lim),
                                    asinh(upper_lim),
                                    m = 4))
  breaks <- round_func(breaks/accuracy) * accuracy
  breaks <- as.character(breaks)
  for (i in seq_along(breaks)) {
    if (as.numeric(breaks[i]) > 1e4 || as.numeric(breaks[i]) < -1e4) {
      breaks[i] <- scales::scientific(as.numeric(breaks[i]), digits = 2)
      breaks[i] <- gsub(pattern = "e\\+0", replacement = "e+", x = breaks[i])
    }
  }
  return(breaks)
}
