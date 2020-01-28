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


get_template_ids_all_versions <- function(){
  # Get template IDs for all accessible CivisML versions.

  template_alias_objects <- fetch_until(aliases_list,
                                       limit=1000,
                                       object_type='template_script',
                                       function(x) length(x) == 0)


  template_aliases <- sapply(template_alias_objects, `[[`, 'alias')

  # Template script aliases are not used exclusively for CivisML
  civisml_template_alias_objects <- template_alias_objects[startsWith(template_aliases,
                                                                      'civis-civisml-')]

  ids <- lapply(civisml_template_alias_objects, function(alias_obj){

        id = alias_obj$objectId

        job_type_version = get_job_type_version(alias_obj$alias)

        return(list(id = id,
                    version = job_type_version$version,
                    name = job_type_version$job_type))

    })

  CIVIS_ML_TEMPLATE_IDS <- data.frame(matrix(unlist(ids),
                                  nrow=length(ids),
                                  byrow=T),
                           stringsAsFactors=FALSE)

  names(CIVIS_ML_TEMPLATE_IDS) <- c("id", "version", "name")
  CIVIS_ML_TEMPLATE_IDS[,"id"] <- as.integer(CIVIS_ML_TEMPLATE_IDS[,"id"])


  return(CIVIS_ML_TEMPLATE_IDS)

}

get_job_type_version <- function(alias){
  # Derive the job type and version from the given alias.

  # `alias` is a character vector and length(`alias`) = 1

  # A version-less alias for production, e.g., "civis-civisml-training"
  match_production <- unlist(regmatches(alias, regexec('\\Acivis-civisml-(\\w+)\\Z', alias, perl=TRUE)))
  # A versioned alias, e.g., "civis-civisml-training-v2-3"
  match_v <- unlist(regmatches(alias, regexec('\\Acivis-civisml-(\\w+)-v(\\d+)-(\\d+)\\Z', alias, perl=TRUE)))
  # A special-version alias, e.g., "civis-civisml-training-dev"
  match_special <- unlist(regmatches(alias, regexec('\\Acivis-civisml-(\\w+)-(\\S+[^-])\\Z', alias, perl=TRUE)))


  if (!identical(match_production, character(0))) {

    job_type = match_production[2]
    version = "prod"

  } else if (!identical(match_v, character(0))) {

    job_type = match_v[2]
    version = paste0("v",match_v[3],".",match_v[4])

  } else if (!identical(match_special, character(0))) {

    job_type = match_special[2]
    version = match_special[3]

  } else {

    stop(paste('Unable to parse the job type and version from the CivisML alias:', alias))

  }

  return(list(job_type = job_type, version = version))

}

# returns a version compatible template id for a given training model without API calls.
get_predict_template_id <- function(m) {
  train_id <- m$job$fromTemplateId
  this_version <- CIVIS_ML_TEMPLATE_IDS[CIVIS_ML_TEMPLATE_IDS$id == train_id, "version"]

  #  `prod` points to the same template id as the latest version (e.g. v2.2)
  CIVIS_ML_TEMPLATE_IDS[CIVIS_ML_TEMPLATE_IDS$version == this_version[1] &
                        CIVIS_ML_TEMPLATE_IDS$name == "prediction", "id"]
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
