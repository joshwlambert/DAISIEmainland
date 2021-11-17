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
