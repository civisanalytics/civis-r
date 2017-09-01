
#' Plot a histogram of the predicted scores or probabilities for a CivisML object
#' @param x civis_ml object
#' @param name name of the class (for multiclass output)
#' @param ... unused.
#' @return A \code{ggplot2} plot object is returned invisibly.
#' @export
hist.civis_ml <- function(x, name = NULL, ...) {
  score_array <- get_metric(x, "score_histogram")
  if (is.null(score_array)) stop("Histogram data not available.")
  if (is_multiclass(x)) {
    names <- get_model_data(x, "class_names")
  } else {
    names <- get_model_data(x, "target_columns")
  }
  if (is.array(score_array)) {
    # 3d array to list of 2d arrays
    score_array <- purrr::array_tree(score_array, margin = 1)
  }
  df <- data.frame(do.call(rbind, score_array))
  colnames(df) <- c("low", "count", "up")
  # if there is no rounding, bar widths -> 0 because of small differences between adjacent midpoints.
  df$mp <- round((df$low + df$up)/2, 1)
  df$freq <- df$count/sum(df$count)
  df$names <- rep(names, sapply(score_array, nrow))
  if (!is.null(name)) df <- subset(df, names == name)

  ggplot2::ggplot(df) +
    ggplot2::geom_bar(ggplot2::aes_string(x = "mp", y = "freq"), stat = "identity") +
    ggplot2::xlab("OUT OF SAMPLE SCORES") +
    ggplot2::ylab("DENSITY") +
    ggplot2::scale_y_continuous(labels = percent) +
    ggplot2::facet_grid(~names) +
    ggplot2::theme_classic()
}

#' Y-yhat plot for regression with civis_ml
#' @param x \code{civis_ml} object
#' @param ... unused
#' @return A \code{ggplot2} plot object is returned invisibly.
#' @export
plot.civis_ml_regressor <- function(x, ...) {
  pl <- get_metric(x, "y_yhat_plot")
  if (is.null(pl)) stop("Plotting data not available.")

  # values is a matrix (binned histogram) with y values along the rows, and yhat values on the columns.
  # this matrix is transformed from wide to long for plotting with ggplot2.
  values <- data.frame(pl$values)
  df <- utils::stack(values)
  df$col_id <- as.numeric(df$ind)
  df$row_id <- rep(1:nrow(values), ncol(values))

  # valid y and yhat values are created from their ranges and step sizes.
  y_vals <- round(seq(from = pl$y_range[1], to = pl$y_range[2], by = pl$y_step))
  yhat_vals <- round(seq(from = pl$yhat_range[1], to = pl$yhat_range[2], by = pl$yhat_step))

  df$x <- y_vals[df$row_id]
  df$y <- yhat_vals[df$col_id]
  ggplot2::ggplot(df, ggplot2::aes_string(x = "x", y = "y", fill = "values")) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient("Bin Size", low = "white", high = civisblue) +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = civisyellow) +
    ggplot2::ylab("PREDICTIONS") + ggplot2::xlab("ACTUALS") +
    ggplot2::theme_classic()
}

#' Decile plot for classification with civis_ml
#' @param x \code{civis_ml} object.
#' @param name Name of the class in a multiclass model to plot.
#' @param ... unused.
#' @return A \code{ggplot2} plot object is returned invisibly.
#' @export
plot.civis_ml_classifier <- function(x, name =  NULL, ...) {

  if (is_multitarget(x)) stop("Plotting data not available.")
  if (is_multiclass(x)) {
    names <- get_model_data(x, "class_names")
    decile <- t(get_metric(x, "deciles"))
    name_id <- if (!is.null(name)) which(names == name) else 1
  } else {
    names <- get_model_data(x, "target_columns")
    decile <- matrix(get_metric(x, "deciles"))
    name_id <- 2
  }
  decile_df <- utils::stack(data.frame(decile))
  decile_df$decile <- rep(1:nrow(decile), times = ncol(decile))
  decile_df$names <- rep(names, each = nrow(decile))

  incidence <- get_metric(x, "pop_incidence_true")[name_id]

  if (!is.null(name)) decile_df <- subset(decile_df, names == name)

  ggplot2::ggplot(decile_df, ggplot2::aes_string(x = "decile", y = "values")) +
    ggplot2::geom_bar(stat = 'identity', fill = "gray") +
    ggplot2::geom_hline(yintercept = incidence, color = civisyellow, size = 2, linetype = 8) +
    ggplot2::annotate("text", x = 4, y = incidence + 0.03,
                      label = paste0("Incidence rate: ", percent(incidence))) +
    ggplot2::facet_grid(~names) +
    ggplot2::scale_x_continuous(breaks = c(1:10)) +
    ggplot2::scale_y_continuous(labels = percent) +
    ggplot2::xlab("DECILE BUCKET") +
    ggplot2::ylab("% TARGET IN DECILE") +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme_classic()
}

civisyellow <- "#FFC525"
civisblue <- "#4292C6"
lightblue = "#86CFE8"
lightestgrey = "#F8F8F8"
darkgrey = "#282828"

percent <- function(x) {
  if (length(x) == 0)
    return(character())
  paste0(round(x * 100, 1), "%")
}
