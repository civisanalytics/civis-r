#' Interface for modeling in the Civis Platform
#'
#' @description An interface for training and scoring data on Civis Platform
#' using a set of Scikit-Learn estimators.
#'
#' @param object A \code{civis_ml} object.
#' @param model_id The \code{id} of CivisML model built previously.
#' @param run_id Optional, the \code{id} of a CivisML model run. If \code{NULL},
#'   defaults to fetching the latest run.
#' @param x,newdata See the Data Sources section below.
#' @param model_type The name of the CivisML workflow. See the Workflows section
#'   below.
#' @param dependent_variable The dependent variable of the training dataset.
#'   For a multi-target problem, this should be a vector of column names of
#'   dependent variables.
#' @param primary_key Optional, the unique ID (primary key) of the training
#'   dataset. This will be used to index the out-of-sample scores. In
#'   \code{predict.civis_ml}, the primary_key of the training task is used by
#'   default \code{primary_key = NA}. Use \code{primary_key = NULL} to
#'   explicitly indicate the data have no primary_key.
#' @param parameters Optional, parameters for the final stage estimator in a
#'   predefined model, e.g. \code{list(C = 2)} for a "sparse_logistic"
#'   model.
#' @param cross_validation_parameters Optional, parameter grid for learner
#'   parameters, e.g. \code{list(n_estimators = c(100, 200, 500),
#'   learning_rate = c(0.01, 0.1), max_depth = c(2, 3))}.
#' @param model_name Optional, the prefix of the Platform modeling jobs.
#'   It will have \code{" Train"} or \code{" Predict"} added to become the Script title.
#' @param calibration Optional, if not \code{NULL}, calibrate output
#'   probabilities with the selected method, \code{sigmoid}, or \code{isotonic}.
#'   Valid only with classification models.
#' @param excluded_columns Optional, a vector of columns which will be
#'   considered ineligible to be independent variables.
#' @param oos_scores_table Optional, if provided, store out-of-sample
#'   predictions on training set data to this Redshift "schema.tablename".
#' @param oos_scores_db Optional, the name of the database where the
#'   \code{oos_scores_table} will be created. If not provided, this will default
#'   to \code{database_name}.
#' @param oos_scores_if_exists Optional, action to take if
#'   \code{oos_scores_table} already exists. One of \code{"fail"}, \code{"append"}, \code{"drop"}, or \code{"truncate"}.
#'   The default is \code{"fail"}.
#' @param fit_params Optional, a mapping from parameter names in the model's
#'   \code{fit} method to the column names which hold the data, e.g.
#'   \code{list(sample_weight = 'survey_weight_column')}.
#' @param output_table The table in which to put predictions.
#' @param output_db The database containing \code{output_table}. If not
#'   provided, this will default to the \code{database_name} specified when
#'   the model was built.
#' @param if_output_exists Action to take if the prediction table already exists. One of \code{"fail"}, \code{"append"}, \code{"drop"}, or \code{"truncate"}.
#'   The default is \code{"fail"}.
#' @param n_jobs  Number of concurrent Platform jobs to use for
#'   multi-file / large table prediction.
#' @param cpu_requested Optional, the number of CPU shares requested in the
#'   Civis Platform for training jobs. 1024 shares = 1 CPU.
#' @param memory_requested Optional, the memory requested from Civis Platform
#'   for training jobs, in MiB.
#' @param disk_requested Optional, the disk space requested on Civis Platform
#'   for training jobs, in GB.
#' @param notifications Optional, model status notifications. See
#'   \code{\link{scripts_post_custom}} for further documentation about email
#'   and URL notification.
#' @param polling_interval Check for job completion every this number of seconds.
#' @param verbose Optional, If \code{TRUE}, supply debug outputs in Platform
#'   logs and make prediction child jobs visible.
#' @param \dots Unused
#'
#' @section CivisML Workflows:
#'
#' You can use the following pre-defined models with \code{civis_ml}. All models
#' start by imputing missing values with the mean of non-null values in a
#' column. The \code{"sparse_*"} models include a LASSO regression step
#' (using \code{glmnet}) to do feature selection before passing data to the
#' final model. In some models, CivisML uses default parameters from those in
#' \href{http://scikit-learn.org/stable/}{Scikit-Learn}.
#' Specific workflows can also be called directly using the R workflow functions.
#'
#' \tabular{rrrrr}{
#'  Name \tab R Workflow \tab Model Type \tab Algorithm \tab Altered Defaults \cr
#'  \code{sparse_logistic}	\tab \code{\link{civis_ml_sparse_logistic}} \tab classification	\tab \href{http://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LogisticRegression.html}{LogisticRegression}	\tab \code{C=499999950, tol=1e-08} \cr
#'  \code{gradient_boosting_classifier} \tab	\code{\link{civis_ml_gradient_boosting_classifier}} \tab classification \tab	\href{http://scikit-learn.org/stable/modules/generated/sklearn.ensemble.GradientBoostingClassifier.html}{GradientBoostingClassifier} \tab	\code{n_estimators=500, max_depth=2} \cr
#'  \code{random_forest_classifier} \tab	\code{\link{civis_ml_random_forest_classifier}} \tab classification \tab	\href{http://scikit-learn.org/stable/modules/generated/sklearn.ensemble.RandomForestClassifier.html}{RandomForestClassifier} \tab	\code{n_estimators=500} \cr
#'  \code{extra_trees_classifier} \tab	\code{\link{civis_ml_extra_trees_classifier}} \tab classification \tab	\href{http://scikit-learn.org/stable/modules/generated/sklearn.ensemble.ExtraTreesClassifier.html}{ExtraTreesClassifier} \tab	\code{n_estimators=500} \cr
#'  \code{sparse_linear_regressor} \tab \code{\link{civis_ml_sparse_linear_regressor}} \tab	regression \tab	\href{http://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LinearRegression.html}{LinearRegression} \tab \cr
#'  \code{sparse_ridge_regressor} \tab	\code{\link{civis_ml_sparse_ridge_regressor}} \tab regression \tab	\href{http://scikit-learn.org/stable/modules/generated/sklearn.linear_model.Ridge.html}{Ridge} \tab \cr
#'  \code{gradient_boosting_regressor}	\tab \code{\link{civis_ml_gradient_boosting_regressor}} \tab regression \tab \href{http://scikit-learn.org/stable/modules/generated/sklearn.ensemble.GradientBoostingRegressor.html}{GradientBoostingRegressor} \tab \code{n_estimators=500, max_depth=2} \cr
#'  \code{random_forest_regressor}	\tab \code{\link{civis_ml_random_forest_regressor}} \tab regression \tab \href{http://scikit-learn.org/stable/modules/generated/sklearn.ensemble.RandomForestRegressor.html}{RandomForestRegressor} \tab \code{n_estimators=500} \cr
#'  \code{extra_trees_regressor} \tab \code{\link{civis_ml_extra_trees_regressor}} \tab regression	\tab \href{http://scikit-learn.org/stable/modules/generated/sklearn.ensemble.ExtraTreesRegressor.html}{ExtraTreesRegressor} \tab \code{n_estimators=500} \cr
#' }
#'
#' @section Data Sources:
#'
#' For building models with \code{civis_ml}, the training data can reside in
#' four different places, a file in the Civis Platform, a CSV file on the local
#' disk, a \code{data.frame} resident in local the R environment, and finally,
#' a table in the Civis Platform. Use the following helpers to specify the
#' data source when calling \code{civis_ml}:
#'
#' \describe{
#'   \item{\code{data.frame}}{\code{civis_ml(x = df, ...)}}
#'   \item{local csv file}{\code{civis_ml(x = "path/to/data.csv", ...)}}
#'   \item{file in Civis Platform}{\code{civis_ml(x = civis_file(1234))}}
#'   \item{table in Civis Platform}{\code{civis_ml(x = civis_table(table_name = "schema.table", database_name = "database"))}}
#' }
#'
#' @section Out of sample scores:
#' Model outputs will always contain out-of-sample (or out of fold) scores,
#' which are accessible through \code{\link{fetch_oos_scores}}.
#' These may be stored in a Civis table on Redshift using the
#' \code{oos_scores}, \code{oos_scores_db}, and \code{oos_scores_if_exists} parameters.
#'
#' @section Predictions:
#'
#' A fitted model can be used to make predictions for data residing in any of
#' the sources above and a \code{\link{civis_file_manifest}}. Similar to
#' \code{civis_ml}, use the data source helpers as the \code{newdata} argument
#' to \code{predict.civis_ml}.
#'
#' A manifest file is a JSON file which specifies the location of many shards of the data to be used for prediction.
#' A manifest file is the output of a Civis export job with \code{force_multifile = TRUE} set, e.g.
#' from \code{\link{civis_to_multifile_csv}}. Large civis tables (provided using \code{table_name})
#' will automatically be exported to manifest files.
#'
#' Prediction outputs will always be stored as gzipped CSVs in one or more civis files.
#' Provide an \code{output_table} (and optionally an \code{output_db},
#' if it's different from \code{database_name}) to copy these predictions into a
#' table on Redshift.
#'
#' @return A \code{civis_ml} object, a list containing the following elements:
#' \item{job}{job metadata from \code{\link{scripts_get_custom}}.}
#' \item{run}{run metadata from \code{\link{scripts_get_custom_runs}}.}
#' \item{outputs}{CivisML metadata from \code{\link{scripts_list_custom_runs_outputs}} containing the locations of
#'  files produced by CivisML e.g. files, projects, metrics, model_info, logs, predictions, and estimators.}
#' \item{metrics}{Parsed CivisML output from \code{metrics.json} containing metadata from validation.
#'  A list containing the following elements:
#'   \itemize{
#'   \item run list, metadata about the run.
#'   \item data list, metadata about the training data.
#'   \item model list, the fitted scikit-learn model with CV results.
#'   \item metrics list, validation metrics (accuracy, confusion, ROC, AUC, etc).
#'   \item warnings list.
#'   \item data_platform list, training data location.
#' }}
#' \item{model_info}{Parsed CivisML output from \code{model_info.json} containing metadata from training.
#'  A list containing the following elements:
#'   \itemize{
#'   \item run list, metadata about the run.
#'   \item data list, metdata about the training data.
#'   \item model list, the fitted scikit-learn model.
#'   \item metrics empy list.
#'   \item warnings list.
#'   \item data_platform list, training data location.
#'   }}
#'
#' @examples \dontrun{
#' # From a data frame:
#' m <- civis_ml(df, model_type = "sparse_logistic",
#'               dependent_variable = "Species")
#'
#' # From a table:
#' m <- civis_ml(civis_table("schema.table", "database_name"),
#'               model_type = "sparse_logistic", dependent_variable = "Species",
#'               oos_scores_table = "schema.scores_table",
#'               oos_scores_if_exists = "drop")
#'
#' # From a local file:
#' m <- civis_ml("path/to/file.csv", model_type = "sparse_logistic",
#'               dependent_variable = "Species")
#'
#' # From a Civis file:
#' file_id <- write_civis_file("path/to/file.csv", name = "file.csv")
#' m <- civis_ml(civis_file(file_id), model_type = "sparse_logistic",
#'               dependent_variable = "Species")
#'
#' pred_job <- predict(m, newdata = df)
#' pred_job <- predict(m, civis_table("schema.table", "database_name"),
#'                     output_table = "schema.scores_table")
#' pred_job <- predict(m, civis_file(file_id),
#'                     output_table = "schema.scores_table")
#'
#' m <- civis_ml_fetch_existing(model_id = m$job$id, m$run$id)
#' logs <- fetch_logs(m)
#' yhat <- fetch_oos_scores(m)
#' yhat <- fetch_predictions(pred_job)
#' }
#' @name civis_ml
#' @seealso
#'   \code{\link{civis_file}}, \code{\link{civis_table}}, and
#'   \code{\link{civis_file_manifest}} for specifying data sources.
#'
#'   \code{\link{fetch_logs}} for retrieving logs for a model build,
#'   \code{\link{fetch_oos_scores}} for retrieving the out of sample (fold) scores for each training observation, and
#'   \code{\link{fetch_predictions}} for retrieving the predictions from a prediction job.
NULL

#' @rdname civis_ml
#' @export
civis_ml <- function(x,
                     dependent_variable,
                     model_type,
                     primary_key = NULL,
                     excluded_columns = NULL,
                     parameters = NULL,
                     fit_params = NULL,
                     cross_validation_parameters = NULL,
                     calibration = NULL,
                     oos_scores_table = NULL,
                     oos_scores_db = NULL,
                     oos_scores_if_exists = c('fail', 'append', 'drop', 'truncate'),
                     model_name = NULL,
                     cpu_requested = NULL,
                     memory_requested = NULL,
                     disk_requested = NULL,
                     notifications = NULL,
                     polling_interval = NULL,
                     verbose = FALSE) {

  UseMethod("civis_ml", x)
}

#' @export
civis_ml.data.frame <- function(x,
                                dependent_variable,
                                model_type,
                                primary_key = NULL,
                                excluded_columns = NULL,
                                parameters = NULL,
                                fit_params = NULL,
                                cross_validation_parameters = NULL,
                                calibration = NULL,
                                oos_scores_table = NULL,
                                oos_scores_db = NULL,
                                oos_scores_if_exists = c('fail', 'append', 'drop', 'truncate'),
                                model_name = NULL,
                                cpu_requested = NULL,
                                memory_requested = NULL,
                                disk_requested = NULL,
                                notifications = NULL,
                                polling_interval = NULL,
                                verbose = FALSE) {

  oos_scores_if_exists <- match.arg(oos_scores_if_exists)

  oos_scores_db_id <- NULL
  if (!is.null(oos_scores_db)) {
    oos_scores_db_id <- get_database_id(oos_scores_db)
  }

  tmp_path <- tempfile()
  utils::write.csv(x, file = tmp_path, row.names = FALSE)
  file_id <- write_civis_file(tmp_path, "modelpipeline_data.csv")
  create_and_run_model(file_id = file_id,
                       dependent_variable = dependent_variable,
                       excluded_columns = excluded_columns,
                       primary_key = primary_key,
                       model_type = model_type,
                       parameters = parameters,
                       cross_validation_parameters = cross_validation_parameters,
                       fit_params = fit_params,
                       calibration = calibration,
                       oos_scores_table = oos_scores_table,
                       oos_scores_db_id = oos_scores_db_id,
                       oos_scores_if_exists = oos_scores_if_exists,
                       model_name = model_name,
                       cpu_requested = cpu_requested,
                       memory_requested = memory_requested,
                       disk_requested = disk_requested,
                       notifications = notifications,
                       verbose = verbose)
}

#' @export
civis_ml.civis_table <- function(x,
                                 dependent_variable,
                                 model_type,
                                 primary_key = NULL,
                                 excluded_columns = NULL,
                                 parameters = NULL,
                                 fit_params = NULL,
                                 cross_validation_parameters = NULL,
                                 calibration = NULL,
                                 oos_scores_table = NULL,
                                 oos_scores_db = NULL,
                                 oos_scores_if_exists = c('fail', 'append', 'drop', 'truncate'),
                                 model_name = NULL,
                                 cpu_requested = NULL,
                                 memory_requested = NULL,
                                 disk_requested = NULL,
                                 notifications = NULL,
                                 polling_interval = NULL,
                                 verbose = FALSE) {

  oos_scores_if_exists <- match.arg(oos_scores_if_exists)

  oos_scores_db_id <- NULL
  if (!is.null(oos_scores_db)) {
    oos_scores_db_id <- get_database_id(oos_scores_db)
  }

  create_and_run_model(table_name = x$table_name,
                       database_id = x$database_id,
                       sql_where = x$sql_where,
                       sql_limit = x$sql_limit,
                       dependent_variable = dependent_variable,
                       excluded_columns = excluded_columns,
                       primary_key = primary_key,
                       model_type = model_type,
                       parameters = parameters,
                       cross_validation_parameters = cross_validation_parameters,
                       fit_params = fit_params,
                       calibration = calibration,
                       oos_scores_table = oos_scores_table,
                       oos_scores_db_id = oos_scores_db_id,
                       oos_scores_if_exists = oos_scores_if_exists,
                       model_name = model_name,
                       cpu_requested = cpu_requested,
                       memory_requested = memory_requested,
                       disk_requested = disk_requested,
                       notifications = notifications,
                       verbose = verbose)
}

#' @export
civis_ml.civis_file <- function(x,
                                dependent_variable,
                                model_type,
                                primary_key = NULL,
                                excluded_columns = NULL,
                                parameters = NULL,
                                fit_params = NULL,
                                cross_validation_parameters = NULL,
                                calibration = NULL,
                                oos_scores_table = NULL,
                                oos_scores_db = NULL,
                                oos_scores_if_exists = c('fail', 'append', 'drop', 'truncate'),
                                model_name = NULL,
                                cpu_requested = NULL,
                                memory_requested = NULL,
                                disk_requested = NULL,
                                notifications = NULL,
                                polling_interval = NULL,
                                verbose = FALSE) {

  oos_scores_if_exists <- match.arg(oos_scores_if_exists)

  oos_scores_db_id <- NULL
  if (!is.null(oos_scores_db)) {
    oos_scores_db_id <- get_database_id(oos_scores_db)
  }

  create_and_run_model(file_id = x,
                       dependent_variable = dependent_variable,
                       excluded_columns = excluded_columns,
                       primary_key = primary_key,
                       model_type = model_type,
                       parameters = parameters,
                       cross_validation_parameters = cross_validation_parameters,
                       fit_params = fit_params,
                       calibration = calibration,
                       oos_scores_table = oos_scores_table,
                       oos_scores_db_id = oos_scores_db_id,
                       oos_scores_if_exists = oos_scores_if_exists,
                       model_name = model_name,
                       cpu_requested = cpu_requested,
                       memory_requested = memory_requested,
                       disk_requested = disk_requested,
                       notifications = notifications,
                       verbose = verbose)
}

#' @export
civis_ml.character <- function(x,
                               dependent_variable,
                               model_type,
                               primary_key = NULL,
                               excluded_columns = NULL,
                               parameters = NULL,
                               fit_params = NULL,
                               cross_validation_parameters = NULL,
                               calibration = NULL,
                               oos_scores_table = NULL,
                               oos_scores_db = NULL,
                               oos_scores_if_exists = c('fail', 'append', 'drop', 'truncate'),
                               model_name = NULL,
                               cpu_requested = NULL,
                               memory_requested = NULL,
                               disk_requested = NULL,
                               notifications = NULL,
                               polling_interval = NULL,
                               verbose = FALSE) {

  oos_scores_if_exists <- match.arg(oos_scores_if_exists)

  oos_scores_db_id <- NULL
  if (!is.null(oos_scores_db)) {
    oos_scores_db_id <- get_database_id(oos_scores_db)
  }

  file_id <- write_civis_file(x, "modelpipeline_data.csv")
  create_and_run_model(file_id = file_id,
                       dependent_variable = dependent_variable,
                       excluded_columns = excluded_columns,
                       primary_key = primary_key,
                       model_type = model_type,
                       parameters = parameters,
                       cross_validation_parameters = cross_validation_parameters,
                       fit_params = fit_params,
                       calibration = calibration,
                       oos_scores_table = oos_scores_table,
                       oos_scores_db_id = oos_scores_db_id,
                       oos_scores_if_exists = oos_scores_if_exists,
                       model_name = model_name,
                       cpu_requested = cpu_requested,
                       memory_requested = memory_requested,
                       disk_requested = disk_requested,
                       notifications = notifications,
                       verbose = verbose)
}

create_and_run_model <- function(file_id = NULL,
                                 table_name = NULL,
                                 database_id = NULL,
                                 sql_where = NULL,
                                 sql_limit = NULL,
                                 dependent_variable = NULL,
                                 excluded_columns = NULL,
                                 primary_key = NULL,
                                 model_type = NULL,
                                 parameters = NULL,
                                 cross_validation_parameters = NULL,
                                 fit_params = NULL,
                                 calibration = NULL,
                                 oos_scores_table = NULL,
                                 oos_scores_db_id = NULL,
                                 oos_scores_if_exists = NULL,
                                 model_name = NULL,
                                 cpu_requested = NULL,
                                 memory_requested = NULL,
                                 disk_requested = NULL,
                                 notifications = NULL,
                                 verbose = FALSE) {

  args <- list(
    MODEL = model_type,
    TARGET_COLUMN = paste(dependent_variable, collapse = " "),
    PRIMARY_KEY = primary_key,
    PARAMS = jsonlite::toJSON(parameters, auto_unbox = TRUE, null = "null"),
    CVPARAMS = jsonlite::toJSON(cross_validation_parameters, null = "null"),
    IF_EXISTS = oos_scores_if_exists,
    TABLE_NAME = table_name,
    # We unclass the file_id here b/c jsonlite::toJSON does not know how to
    # seralize our custom class. This results in the following error:
    #  Error: No method asJSON S3 class: civis_file
    # It does not appear the maintainers have any interest in allowing users
    # to specify/override the asJSON method:
    #   https://github.com/jeroen/jsonlite/issues/62
    CIVIS_FILE_ID = unclass(file_id),
    DEBUG = verbose
  )

  if (!is.null(calibration)) {
    if (!(calibration %in% c("sigmoid", "isotonic"))) {
      stop("calibration must be 'sigmoid', 'isotonic', or NULL.")
    }
    args[["CALIBRATION"]] <- calibration
  }

  if (!is.null(oos_scores_table)) {
    args[["OOSTABLE"]] <- oos_scores_table
    args[["OOSDB"]] <- list(database = oos_scores_db_id)
  }

  if (!is.null(sql_where)) {
    args[["WHERESQL"]] <- sql_where
  }

  if (!is.null(sql_limit)) {
    args[["LIMITSQL"]] <- sql_limit
  }

  if (!is.null(excluded_columns)) {
    args[["EXCLUDE_COLS"]] <- paste(excluded_columns, collapse = " ")
  }

  if (!is.null(fit_params)) {
    args[["FIT_PARAMS"]] <- jsonlite::toJSON(fit_params, auto_unbox = TRUE)
  }

  if (!is.null(database_id)) {
    # Prior to v1.0, this parameter was DB_NAME
    args[["DB"]] <- list(database = database_id)
  }

  if (!is.null(cpu_requested)) {
    args[["REQUIRED_CPU"]] <- cpu_requested
  }

  if (!is.null(memory_requested)) {
    args[["REQUIRED_MEMORY"]] <- memory_requested
  }

  if (!is.null(disk_requested)) {
    args[["REQUIRED_DISK_SPACE"]] <- disk_requested
  }

  args <- I(args)  # We don't want any conversions and by toJSON.

  job_name <- NULL
  if (!is.null(model_name)) {
    job_name <- paste0(model_name, " Train")
  }

  tmpl_id <- getOption("civis.ml_train_template_id")
  run <- run_model(template_id = tmpl_id, name = job_name, arguments = args,
                   notifications = notifications, verbose = verbose)
  civis_ml_fetch_existing(run$job_id, run$run_id)
}

run_model <- function(template_id, name, arguments, notifications, verbose) {
  script_args <- list(
    from_template_id = template_id,
    arguments = arguments
  )

  # We must remove both `name` and `notifactions` when NULL, otherwise, we get
  # a 500 error from platform.
  if (!is.null(name)) {
    script_args$name <- name
  }

  if (!is.null(notifications)) {
    script_args$notifications <- notifications
  }

  job <- do.call(scripts_post_custom, script_args)
  run <- scripts_post_custom_runs(job$id)
  r <- tryCatch(await(scripts_get_custom_runs, id = job$id, run_id = run$id,
                      .verbose = verbose),
                civis_error = function(e) stop(civis_ml_error(e)),
                error = function(e) stop(e))
  list(job_id = job$id, run_id = run$id)
}

#' @rdname civis_ml
#' @export
civis_ml_fetch_existing <- function(model_id, run_id = NULL) {
  job <- must_fetch_civis_ml_job(model_id)

  run_id <- run_id %||% job$lastRun$id
  if (is.null(run_id)) {
    stop("Error: invalid model task.")
  }
  run <- must_fetch_civis_ml_run(model_id, run_id)

  outputs <- metrics <- model_info <- NULL
  if (run$state == "succeeded") {
    outputs <- scripts_list_custom_runs_outputs(id = model_id, run_id = run_id)
    metrics <- must_fetch_output_json(outputs, "metrics.json")
    model_info <- must_fetch_output_json(outputs, "model_info.json")
  }
  type <- model_type(job)

  structure(
    list(
      job = job,
      run = run,
      outputs = outputs,
      metrics = metrics,
      model_info = model_info
    ),
    class = c(paste0("civis_ml_", type), "civis_ml")
  )
}

must_fetch_civis_ml_job <- function(model_id) {
  tryCatch(scripts_get_custom(model_id),
    "http_404" = function(e) stop(paste0("Error: model ", model_id, " not found."), call. = FALSE)
  )
}

must_fetch_civis_ml_run <- function(model_id, run_id) {
  run <- tryCatch(scripts_get_custom_runs(model_id, run_id),
    "http_404" = function(e) stop(paste0("Error: run ", run_id, " not found."), call. = FALSE)
  )

  switch(run$state,
         "failed" = warning("The model task failed, use fetch_logs to retreive any error messages."),
         "cancelled" = warning("The model task was cancelled."),
         "queued" = message("The model task queued."),
         "running" = message("The model task is still running."))

  run
}

must_fetch_output_json <- function(outputs, file_name) {
  path <- must_fetch_output_file(outputs, file_name)
  jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
}

must_fetch_output_file <- function(outputs, file_name) {
  out <- purrr::keep(outputs, ~ .$objectType == "File" && .$name == file_name)
  if (purrr::is_empty(out)) {
    stop(paste0(file_name, " not found in model output."), call. = FALSE)
  }

  # There is no strict requirement on the file names being unique *and* we want
  # to return a non-nested list anyway.
  file_output <- out[[1]]
  download_civis(file_output$objectId, tempfile())
}

#' @rdname civis_ml
#' @export
predict.civis_ml <- function(object,
                             newdata,
                             primary_key = NA,
                             output_table = NULL,
                             output_db = NULL,
                             if_output_exists = c('fail', 'append', 'drop', 'truncate'),
                             n_jobs = NULL,
                             polling_interval = NULL,
                             verbose = FALSE,
                             ...) {

  output_db_id <- NULL
  if (!is.null(output_db)) {
    output_db_id <- get_database_id(output_db)
  }
  if_output_exists <- match.arg(if_output_exists)
  # TODO: normalize this...
  model_name <- object$job$name

  if (!is.null(primary_key) && is.na(primary_key)) {
    primary_key <- object$job$arguments$PRIMARY_KEY
  }

  pred_args <- list(
    train_job_id = object$job$id,
    train_run_id = object$run$id,
    primary_key = primary_key,
    output_table = output_table,
    output_db_id = output_db_id,
    if_output_exists = if_output_exists,
    model_name = model_name,
    n_jobs = n_jobs,
    polling_interval = polling_interval,
    verbose = verbose
  )

  if (inherits(newdata, "data.frame")) {
    tmp_path <- tempfile()
    utils::write.csv(newdata, file = tmp_path, row.names = FALSE)
    file_id <- write_civis_file(tmp_path, "modelpipeline_data.csv")
    pred_args[["file_id"]] <- file_id
  }

  if (is.character(newdata)) {
    file_id <- write_civis_file(newdata, "modelpipeline_data.csv")
    pred_args[["file_id"]] <- file_id
  }

  if (inherits(newdata, "civis_file")) {
    # See above, we need to strip class attribute for jsonlite::toJSON.
    pred_args[["file_id"]] <- unclass(newdata)
  }

  if (inherits(newdata, "civis_file_manifest")) {
    # See above, we need to strip class attribute for jsonlite::toJSON.
    pred_args[["manifest"]] <- unclass(newdata)
  }

  if (inherits(newdata, "civis_table")) {
    pred_args[["table_name"]] <- newdata$table_name
    pred_args[["database_id"]] <- newdata$database_id
    pred_args[["sql_where"]] <- newdata$sql_where
    pred_args[["sql_limit"]] <- newdata$sql_limit
  }

  do.call(create_and_run_pred, pred_args)
}

create_and_run_pred <- function(train_job_id = NULL,
                                train_run_id = NULL,
                                file_id = NULL,
                                table_name = NULL,
                                database_id = NULL,
                                sql_where = NULL,
                                sql_limit = NULL,
                                manifest = NULL,
                                primary_key = NULL,
                                output_table = NULL,
                                output_db_id = NULL,
                                if_output_exists = NULL,
                                model_name = NULL,
                                n_jobs = NULL,
                                polling_interval = NULL,
                                notifications = NULL,
                                verbose = FALSE) {
  args <- list(
    TRAIN_JOB = train_job_id,
    TRAIN_RUN = train_run_id,
    PRIMARY_KEY = primary_key,
    IF_EXISTS = if_output_exists,
    N_JOBS = n_jobs,
    DEBUG = verbose,
    CIVIS_FILE_ID = file_id
  )

  if (!is.null(database_id)) {
    args[["TABLE_NAME"]] <- table_name
    args[["DB"]] <- list(database = database_id)
  }

  if (!is.null(sql_where)) {
    args[["WHERESQL"]] <- sql_where
  }

  if (!is.null(sql_limit)) {
    args[["LIMITSQL"]] <- sql_limit
  }

  if (!is.null(output_table)) {
    args[["OUTPUT_TABLE"]] <- output_table
    args[["OUTPUT_DB"]] <- list(database = output_db_id)
  }

  if (!is.null(manifest)) {
    args[["MANIFEST"]] <- manifest
  }

  if (!is.null(n_jobs) && n_jobs == 1) {
    args[["REQUIRED_CPU"]] <- 1024
    args[["REQUIRED_MEMORY"]] <- 3000
    args[["REQUIRED_DISK_SPACE"]] <- 30
  }

  args <- I(args)

  job_name <- NULL
  if (!is.null(model_name)) {
    job_name <- paste0(model_name, " Predict")
  }

  tmpl_id <- getOption("civis.ml_predict_template_id")
  run <- run_model(template_id = tmpl_id, name = job_name, arguments = args,
                   notifications = notifications, verbose = verbose)
  fetch_predict_results(run$job_id, run$run_id)
}

fetch_predict_results <- function(job_id, run_id) {
  job <- scripts_get_custom(job_id)
  run <- scripts_get_custom_runs(job_id, run_id)
  outputs <- scripts_list_custom_runs_outputs(job_id, run_id)
  model_info <- must_fetch_output_json(outputs, "model_info.json")

  structure(
    list(
      job = job,
      run = run,
      outputs = outputs,
      model_info = model_info
    ),
    class = c("civis_ml_prediction", "civis_ml")
  )
}

#' Retrieve predictions from a CivisML prediction job
#' @param x \code{civis_ml_prediction} object from \code{predict.civis_ml}
#' @param ... arguments passed to \code{read.csv}
#' @export
#' @details The resulting file can also be downloaded directly using \code{download_civis} (see examples).
#' @return A \code{data.frame} with out of sample/fold predictions for each
#'  row of the training data, and containing an additional column with
#'  a primary key. For a multiclass model, a data frame is returned with one
#'   column of predictions for each class.
#'
#' @examples
#' \dontrun{
#' m <- civis_ml("path/to/file.csv", model_type = "sparse_logistic",
#'   dependent_variable = "Species")
#' pred_job <- predict(m, newdata = "path/to/newdata.csv")
#' yhat <- fetch_predictions(pred_job)
#'
#' # download instead:
#' download_civis(pred_job$model_info$output_file_ids, path = "my_predictions.csv")
#'
#' }
fetch_predictions <- function(x, ...) {
  stopifnot(is(x, "civis_ml_prediction"))
  out <- fetch_predict_results(job_id = x$job$id, run_id = x$run$id)
  # there will always be one file id - this is a pointer to the url shards on S3 (src: Jamie)
  id <- out$model_info$output_file_ids
  tryCatch({
    path <- download_civis(id, tempfile())
    res <- utils::read.csv(path, ...)
  }, error = function(e) stop(e),
  finally = {
    unlink(path)
  })
  res
}

#' A file in the Civis Platform
#'
#' Use \code{civis_file} to use a file in the Civis Platform with
#'   \link{civis_ml}.
#'
#' @param file_id The id of a Civis file.
#' @return A \code{civis_file}.
#' @export
civis_file <- function(file_id) {
  structure(file_id, class = "civis_file")
}

#' A table in the Civis Platform
#'
#' Use \code{civis_table} to use a table in the Civis Platform with
#' \link{civis_ml}.
#'
#' @param table_name The table name, as \code{"schema.table"}.
#' @param database_name The name of the database holding \code{table_name}.
#' @param sql_where A \code{SQL WHERE} clause used to scope the rows of the
#'   training or prediction table. Note, the \code{WHERE} keyword is added at
#'   runtime.
#' @param sql_limit A \code{SQL_LIMIT} clause for querying the training or
#'   prediction set. Note, the \code{LIMIT} keyword is added at runtime.
#' @return A \code{civis_table}.
#' @export
civis_table <- function(table_name,
                        database_name = NULL,
                        sql_where = NULL,
                        sql_limit = NULL) {

  database_name <- database_name %||% get_default_database()
  if (is.null(database_name)) {
    stop(paste0("Please supply a database_name or set a global default: ",
         "options(civis.default_database = 'db_name')"),
         call. = FALSE)
  }

  structure(
    list(
      table_name = table_name,
      database_name = database_name,
      database_id = get_database_id(database_name),
      sql_where = sql_where,
      sql_limit = sql_limit
    ),
    class = "civis_table"
  )
}

#' A manifest file in the Civis Platform
#'
#' Use \code{civis_file_manifest} to use a manifest file in the Civis Platform
#' with \link{civis_ml}.
#'
#' @param file_id The id of a Civis file.
#' @return A \code{civis_file_manifest}
#' @export
civis_file_manifest <- function(file_id) {
  structure(file_id, class = "civis_file_manifest")
}

#' @rdname fetch_logs
#' @export
fetch_logs.civis_ml <- function(object, limit = 100, ...) {
  logs <- scripts_list_custom_runs_logs(object$job$id, object$run$id,
                                        limit = limit)
  format_scripts_logs(logs)
}

#' @export
fetch_logs.civis_ml_error <- function(object, limit = 100, ...) {
  job_id <- attr(object, "args")$id
  run_id <- attr(object, "args")$run_id
  logs <- scripts_list_custom_runs_logs(job_id, run_id, limit = limit)
  format_scripts_logs(logs)
}

#' Retrieve out of sample/fold predictions
#'
#' @details Returns the out of sample (or out of fold) predictions for each
#'   observation. For a multiclass model, a data frame is returned with one
#'   column of predictions for each class.
#'
#' @param model A \code{civis_ml} model.
#' @param \dots Parameters passed to \code{read.csv}.
#' @return A \code{data.frame} with out of sample/fold predictions for each
#'  row of the training data, and containing an additional column with
#'  a primary key.
#'
#' @seealso civis_ml
#' @importFrom utils read.csv
#' @export
fetch_oos_scores <- function(model, ...) {
  stopifnot(is_civis_ml(model))
  path <- must_fetch_output_file(model$outputs, "predictions.csv.gz")
  read.csv(path, ...)
}



is_civis_ml <- function(object) {
  is(object, "civis_ml")
}
