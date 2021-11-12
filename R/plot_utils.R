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
create_plot_breaks <- function(lower_lim, upper_lim) {
  breaks <- scales::breaks_log(base = exp(1))(lower_lim:upper_lim)
  return(breaks)
}

#' Creates the axis numbers (labels) for plotting with an inverse hyperbolic
#' sine transformation, with rounding to a set accuracy to reduce decimal
#' places plotted
#'
#' @inheritParams default_params_doc
#'
#' @return Character vector
create_plot_labelss <- function(lower_lim, upper_lim) {
  breaks <- scales::breaks_log(base = exp(1))(lower_lim:upper_lim)
  labels <- as.character(breaks)
  for (i in seq_along(breaks)) {
    if (breaks[i] > 1e4 || breaks[i] < -1e4) {
      labels[i] <- gsub(pattern = "e",
                        replacement = "%*%10<sup>",
                        x = scales::scientific_format()(x))
      labels[i] <- paste0(labels[i], "</sup>")
    }
  }
  return(labels)
}


scientific <- function(x) {
  browser()
  x <- ifelse(x > 1e4 | x < -1e4,
              format(x, digits = 2, scientific = TRUE),
              format(x, digits = 2, scientific = FALSE))
  x <- parse(text = gsub(pattern = "e",
                         replacement = " %*% 10^",
                         x = x))
  return(x)
}

old_scientific <- function(x) {
  parse(text = gsub(pattern = "e",
                    replacement = " %*% 10^",
                    x = choose_scientific(x)))
}

choose_scientific <- function(x) {
  ifelse(x > 1e4,
         format(x, digits = 2, scientific = TRUE),
         format(x, digits = 2, scientific = FALSE))
}

old_choose_scientific <- function(x) {
  for (i in seq_along(x)) {
    if (!is.na(x[i])) {
      if (x[i] > 1e4 || x[i] < -1e4) {
        x[i] <- parse(text = gsub(
          pattern = "e",
          replacement = " %*% 10^",
          x = format(x[i], digits = 2, scientific = TRUE)))
      } else {
        x[i] <- format(x[i], digits = 2, scientific = FALSE)
      }
    }
  }
  return(x)
}

create_plot_labelsss <- function(x) {
  if (!is.na(x)) {
    if (x > 1e4 || x < -1e4) {
      return(parse(text = gsub(
        pattern = "e",
        replacement = " %*% 10^",
        x = scales::scientific_format()(x))))
    } else {
      return(as.character(x))
    }
  }
}
