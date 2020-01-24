# print, summary, plot -----

#' List of civis_ml regression models.
#' @export
CIVIS_ML_REGRESSORS <- c("sparse_linear_regressor",
                         "sparse_ridge_regressor",
                         "gradient_boosting_regressor",
                         "random_forest_regressor",
                         "extra_trees_regressor",
                         "multilayer_perceptron_regressor",
                         "stacking_regressor")

#' List of classification models.
#' @export
CIVIS_ML_CLASSIFIERS <- c("sparse_logistic",
                          "gradient_boosting_classifier",
                          "random_forest_classifier",
                          "extra_trees_classifier",
                          "multilayer_perceptron_classifier",
                          "stacking_classifier")

CIVIS_ML_TEMPLATE_IDS <- data.frame(
  id = c(9112, 9113, 9968, 9969, 10582, 10583, 11219, 11220, 11221),
  version = c(1.1, 1.1, 2.0, 2.0, 2.1, 2.1, 2.2, 2.2, 2.2),
  name = c("train", "predict", "train", "predict", "train", "predict", "train", "predict", "register"),
  stringsAsFactors = FALSE
)


# get_template_ids_all_versions <- function(){
#   # Get template IDs for all accessible CivisML versions.
#   #
#   # Args:
#   #
#   # Returns:
#   #   CIVIS_ML_TEMPLATE_IDS: A data frame of collections of version, name, IDs


#   template_alias_objects <- aliases_list(object_type='template_script')

#   df_aliases <- data.frame(matrix(unlist(template_alias_objects),
#                                   nrow=length(template_alias_objects),
#                                   byrow=T),
#                            stringsAsFactors=FALSE)

#   names(df_aliases) <- c("id", "objectId", "objectType", "alias", "userId", "displayName")
#   df_aliases[,c("id","objectId","userId")] <- sapply(df_aliases[,c("id","objectId","userId")],as.integer)


# }


# returns a version compatible template id for a given training model without API calls.
get_predict_template_id <- function(m) {
  train_id <- m$job$fromTemplateId
  this_version <- CIVIS_ML_TEMPLATE_IDS[CIVIS_ML_TEMPLATE_IDS$id == train_id, "version"]
  CIVIS_ML_TEMPLATE_IDS[CIVIS_ML_TEMPLATE_IDS$version == this_version &
                        CIVIS_ML_TEMPLATE_IDS$name == "predict", "id"]
}

#' @export
print.civis_ml_classifier <- function(x, digits = 4, ...) {
  class_names <- get_model_data(x, "class_names")
  aucs <- get_metric(x, "roc_auc")
  p_cor <- get_metric(x, "p_correct")
  dv_names <- get_model_data(x, "target_columns")

  wfl <- model_workflow(x)
  url <- model_url(x)
  run_id <- x$run$id
  job_id <- x$job$id

  cat("<CivisML ", wfl, ">\n", sep = "")
  cat(url, fill = TRUE)
  cat("Job id: ", job_id, " Run id: ", run_id, "\n", fill = TRUE)

  if (!is.null(x$metrics)) {
    if (any(is_multiclass(x)) & is_multitarget(x)) {
      tabs <- mapply(class_metric_table, p_cor, class_names, aucs)
    } else if (is_multiclass(x)) {
        tabs <- list(class_metric_table(p_cor, class_names, aucs))
    } else {
      cat("AUC: ", signif(aucs, digits = digits), fill = TRUE)
      tabs <- list(class_metric_table(p_cor, class_names))
    }

    for (i in seq_along(tabs)) {
      cat(paste0(dv_names[i], ":"), fill = TRUE)
      print(signif(tabs[[i]], digits = digits))
      cat("\n")
    }
  }

  invisible(x)
}

class_metric_table <- function(p_correct = NULL, names, auc = NULL) {
  if (!is.null(auc)) {
    tab <- t(cbind(auc, p_correct))
    rownames(tab) <- c("AUC", "Prop Correct")
  } else {
    tab <- t(as.matrix(p_correct))
    rownames(tab) <- c("Prop Correct")
  }
  colnames(tab) <- names
  tab
}

#' @export
print.civis_ml_regressor <- function(x, digits = 4, ...) {
  wfl <- model_workflow(x)
  url <- model_url(x)
  run_id <- x$run$id
  job_id <- x$job$id

  cat("<CivisML ", wfl, ">\n", sep = "")
  cat(url, fill = TRUE)
  cat("Job id: ", job_id, " Run id: ", run_id, "\n", fill = TRUE)

  dv_names <- x$model_info$data$target_columns
  if (!is.null(x$metrics)) {
    tab <- with(x$metrics$metrics, t(cbind(r_squared, rmse, mad)))
    colnames(tab) <- dv_names
    rownames(tab) <- c( "R-squared", "RMSE", "MAD")
    print(signif(tab, digits = digits))
  }
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


#' Get model feature importance
#' @param model Object from \code{\link{civis_ml}}
#' @return A matrix of features and their importance, ranked in descending order of importance
#' @export
get_feature_importance <- function(model){

  if (is.null(model$metrics$model$parameters$feature_importances)) stop("Feature importance data not available.")

  params <- model$metrics$model$parameters
  variable_order <- order(params$feature_importances,
                          decreasing = TRUE)
  variable_name <- params$relvars[variable_order]
  importance <- params$feature_importances[variable_order]

  feature_importance_df <- data.frame('variable_name' = variable_name,
                                      'importance' = importance)
  feature_importance_df
}


get_model_data <- function(model, name = NULL) {
  if (!is.null(name)) {
    model$model_info$data[[name]]
  } else {
    model$model_info$data
  }
}

model_url <- function(x) {
  paste0("https://platform.civisanalytics.com/#/models/", x$job$id)
}

model_type <- function(job) {
  is_regr <- grepl('regressor', job$arguments$MODEL)
  type <- if (is_regr) "regressor" else "classifier"
  type
}

model_workflow <- function(m) {
  m$job$arguments$MODEL
}

is_multiclass <- function(model) {
  (get_model_data(model, "n_unique_targets") > 2) &
    is(model, "civis_ml_classifier")
}

is_multitarget <- function(model) {
  length(get_model_data(model, "n_unique_targets")) > 1
}


#' Get civis_ml model coefficients
#' @details Outputs coefficients with names in the style of `stats::coef`
#' @param object civis_ml_model
#' @param complete see documentation for generic \code{coef()}
#' @param ... other arguments
#' @return a matrix of coefficients, or `NULL` if none available from CivisML
#' @export
coef.civis_ml <- function(object, complete = TRUE, ...) {
  if (is.null(object$model_info$model$parameters$coef)) {
    return(NULL)
  } else {
    intercept <- as.data.frame(object$model_info$model$parameters$intercept)
    non_intercept_coefs <- as.data.frame(matrix(object$model_info$model$parameters$coef,
                                                nrow=nrow(intercept)))
    coefs <- cbind(intercept, non_intercept_coefs)
    colnames(coefs) <- c("(Intercept)", as.vector(object$model_info$model$parameters$relvars))
    if (length(object$model_info$data$class_names) > 2) {    # if multiclass
      rownames(coefs) <- object$model_info$data$class_names
    }
  }
  coefs
}
