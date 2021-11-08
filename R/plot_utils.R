#' Calculates the 5th, 25th, 50th, 75th and 95th percentile of the data
#'
#' @inheritParams default_params_doc
#'
#' @return Named vector of 5 named elements
calc_quantiles <- function(plotting_data) {
  quantiles <- stats::quantile(plotting_data, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
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
      breaks[i] <- gsub(pattern = "e\\+0", replacement = "x10<sup>", x = breaks[i])
      breaks[i] <- paste0(breaks[i], "</sup>")
    }
  }
  return(breaks)
}
