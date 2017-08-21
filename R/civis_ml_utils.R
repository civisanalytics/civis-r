# print, summary, plot -----

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
  tab <- with(x$metrics$metrics, t(cbind(mad, rmse, r_squared)))
  colnames(tab) <- dv_names
  rownames(tab) <- c("MAD", "RMSE", "R-squared")
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
print.predict_civis_ml <- function(x, ...) {
  cat("<CivisML Prediction>\n", sep = "")
}

model_url <- function(x) {
  paste0("https://platform.civisanalytics.com/#/scripts/custom/", x$job$id)
}

model_type <- function(job) {
  is_regr <- stringr::str_detect(job$arguments$MODEL, "regressor")
  type <- if (is_regr) "regressor" else "classifier"
  type
}

model_workflow <- function(m) {
  m$job$arguments$MODEL
}
