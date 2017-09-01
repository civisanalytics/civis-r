# print, summary, plot -----

#' List of civis_ml regression models.
#' @export
CIVIS_ML_REGRESSORS <- c("sparse_linear_regressor", "sparse_ridge_regressor",
                        "gradient_boosting_regressor","random_forest_regressor",
                        "extra_trees_regressor")

#' List of classification models.
#' @export
CIVIS_ML_CLASSIFIERS <- c("sparse_logistic", "gradient_boosting_classifier",
                         "random_forest_classifier",
                         "extra_trees_classifier")

#' @export
print.civis_ml_classifier <- function(x, ...) {
  class_names <- c(x$model_info$data$class_names)
  tab <- with(x$metrics$metrics, t(cbind(roc_auc, p_correct)))
  colnames(tab) <- class_names
  rownames(tab) <- c("AUC", "Prop Correct")
  print.civis_ml(x, tab = tab, ...)
}

#' @export
print.civis_ml_regressor <- function(x, ...) {
  dv_names <- x$model_info$data$target_columns
  tab <- with(x$metrics$metrics, t(cbind(r_squared, rmse, mad)))
  colnames(tab) <- dv_names
  rownames(tab) <- c( "R-squared", "RMSE", "MAD")
  print.civis_ml(x, tab = tab, ...)
}

print.civis_ml <- function(x, tab, digits = 4, ...) {
  wfl <- model_workflow(x)
  url <- model_url(x)
  run_id <- x$run$id
  job_id <- x$job$id
  cat("<CivisML ", wfl, ">\n", sep = "")
  cat(url, fill = TRUE)
  cat("Job id: ", job_id, " Run id: ", run_id, "\n", fill = TRUE)
  print(signif(tab, digits = digits))
  invisible(x)
}

#' @export
print.civis_ml_prediction <- function(x, ...) {
  run_id <- x$run$id
  job_id <- x$job$id
  url <- paste0("https://platform.civisanalytics.com/#/scripts/", job_id)
  cat("<CivisML Prediction>", fill = T)
  cat(url, fill = TRUE)
  cat("Job id: ", job_id, " Run id: ", run_id, "\n", fill = TRUE)
  invisible(x)
}

#' @export
print.civis_ml_error <- function(x, ...) {
  cat("<civis_ml_error>", fill = T)
  cat(x$message, sep = "\n")
}

# wraps a civis_error caught by await specifically for civis_ml.
civis_ml_error <- function(civis_error_obj) {
  orig_msg <- civis_error_obj$message

  log <- fetch_logs.civis_ml_error(civis_error_obj)
  new_msg <- paste0(c(orig_msg, log), collapse = "\n")

  # condition contains all original attributes
  condition(subclass = c("civis_ml_error", "civis_error", "error"),
            message = new_msg, call = NULL,
            log = log,
            f = attr(civis_error_obj, "f"),
            args = attr(civis_error_obj, "args"))
}

#' Get model metrics
#' @param model Object from \code{\link{civis_ml}}
#' @param name Name of the metric. If \code{NULL}, all metrics are returned. Possible metrics are listed in details.
#' @export
#' @return The metric given by \code{name}, or all metrics.
#' @details The list of possible metrics provided by CivisML is:
#'
#' Classification:
#'
#' \code{"accuracy"}, \code{"confusion_matrix"}, \code{"p_correct"}, \code{"pop_incidence_true"},
#' \code{"pop_incidence_pred"}, \code{"roc_auc"}, \code{"log_loss"}, \code{"brier_score"},
#' \code{"roc_curve"}, \code{"calibration_curve"}, \code{"deciles"}, \code{"score_histogram"},
#' \code{"training_histogram"}, \code{"oos_score_table"}.
#'
#' Regression:
#'
#' \code{"mad"}, \code{"rmse"}, \code{"r_squared"}, \code{"score_histogram"}, \code{"training_histogram"},
#' \code{"y_yhat_plot"}, \code{"y_yhat_outlier_rows"}, \code{"oos_score_table"}
#'
get_metric <- function(model, name = NULL) {
  stopifnot(is_civis_ml(model))
  if (!is.null(name)) {
    model$metrics$metrics[[name]]
  } else {
    model$metrics$metrics
  }
}

get_model_data <- function(model, name = NULL) {
  if (!is.null(name)) {
    model$metrics$data[[name]]
  } else {
    model$metrics$data
  }
}

model_url <- function(x) {
  paste0("https://platform.civisanalytics.com/#/models/", x$job$id)
}

model_type <- function(job) {
  is_regr <- stringr::str_detect(job$arguments$MODEL, "regressor")
  type <- if (is_regr) "regressor" else "classifier"
  type
}

model_workflow <- function(m) {
  m$job$arguments$MODEL
}

is_multiclass <- function(model) {
  length(get_model_data(model, "class_names")) > 2
}
