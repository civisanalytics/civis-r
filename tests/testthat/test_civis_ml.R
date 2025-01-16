library(mockery)

context("civis_ml")

ml_train_template_id <- 234
ml_predict_template_id <- 456

test_that("jsonlite works", {
  # The tests below fail when run via R CMD check due with a
  # "invalid encoding argument" error. jsonlite::toJSON is the last thing in
  # the traceback. Adding this test here seems to make the issue go away.
  # TODO: find out why.
  s <- jsonlite::toJSON(list(sample_weight = "survey_weights"), auto_unbox = TRUE)
  expect_equal(unclass(s), "{\"sample_weight\":\"survey_weights\"}")
})

################################################################################
# Build

test_that("calls scripts_post_custom", {
  fake_get_database_id <- mock(456, cycle = TRUE)
  fake_scripts_post_custom <- mock(list(id = 999))
  fake_scripts_post_custom_runs <- mock(list(id = 888))
  fake_scripts_get_custom_runs <- mock(list(state = "running"), list(state = "succeeded"))
  fake_civis_ml_fetch_existing <- mock(NULL)

  local_mocked_bindings(
    get_database_id = fake_get_database_id,
    scripts_post_custom = fake_scripts_post_custom,
    scripts_post_custom_runs = fake_scripts_post_custom_runs,
    scripts_get_custom_runs = fake_scripts_get_custom_runs,
    civis_ml_fetch_existing = fake_civis_ml_fetch_existing,
    get_train_template_id = function(...) ml_train_template_id
  )

  tbl <- civis_table(table_name = "schema.table",
                      database_name = "a_database",
                      sql_where = "1 = 2",
                      sql_limit = 10)

  civis_ml(x = tbl,
            model_type = "sparse_logistic",
            dependent_variable = "target",
            excluded_columns = c("col_1", "col_2", "col_3"),
            primary_key = "row_number",
            parameters = list(n_estimators = 10),
            cross_validation_parameters = list(n_estimators = c(10, 20, 30)),
            model_name = "awesome civisml",
            calibration = "sigmoid",
            oos_scores_table = "score.table",
            oos_scores_db = "another_database",
            oos_scores_if_exists = "drop",
            fit_params = list(sample_weight = "survey_weights"),
            cpu_requested = 1111,
            memory_requested = 9096,
            disk_requested = 9,
            notifications = list(successEmailSubject = "A success",
                                 successEmailAddresses = c("user@example.com")),
            polling_interval = .01,
            validation_data = "skip",
            n_jobs = 9,
            verbose = FALSE)

  script_args <- mock_args(fake_scripts_post_custom)[[1]]
  expect_equal(script_args$from_template_id, ml_train_template_id)
  expect_equal(script_args$name, "awesome civisml Train")
  expect_equal(script_args$notifications, list(successEmailSubject = "A success",
                                               successEmailAddresses = c("user@example.com")))

  # These are template args/params:
  ml_args <- script_args$arguments
  expect_is(ml_args, "AsIs")  # We don't want jsonlite doing anything unexpected.
  expect_equal(ml_args$MODEL, "sparse_logistic")
  expect_equal(ml_args$TARGET_COLUMN, "target")
  expect_equal(ml_args$PRIMARY_KEY, "row_number")
  expect_equal(unclass(ml_args$PARAMS), '{"n_estimators":10}')
  expect_equal(unclass(ml_args$CVPARAMS), '{"n_estimators":[10,20,30]}')
  expect_equal(ml_args$CALIBRATION, "sigmoid")
  expect_equal(ml_args$IF_EXISTS, "drop")
  expect_equal(ml_args$TABLE_NAME, "schema.table")
  expect_equal(ml_args$CIVIS_FILE_ID, NULL)
  expect_equal(ml_args$OOSTABLE, "score.table")
  expect_equal(ml_args$OOSDB, list(database = 456))
  expect_equal(ml_args$WHERESQL, "1 = 2")
  expect_equal(ml_args$LIMITSQL, 10)
  expect_equal(ml_args$EXCLUDE_COLS, "col_1 col_2 col_3")
  expect_equal(unclass(ml_args$FIT_PARAMS), '{"sample_weight":"survey_weights"}')
  expect_equal(ml_args$DB, list(database = 456))
  expect_equal(ml_args$REQUIRED_CPU, 1111)
  expect_equal(ml_args$REQUIRED_MEMORY, 9096)
  expect_equal(ml_args$REQUIRED_DISK_SPACE, 9)
  expect_equal(ml_args$VALIDATION_DATA, "skip")
  expect_equal(ml_args$N_JOBS, 9)

  # Make sure we started the job.
  expect_args(fake_scripts_post_custom_runs, 1, 999)

  # And checked it's status
  expect_args(fake_scripts_get_custom_runs, 1, 999, 888)
  expect_called(fake_scripts_get_custom_runs, 2)
})

test_that("calls civis_ml.data.frame for local df", {
  fake_write_civis_file <- mock(1234)
  fake_get_database_id <- mock(456)
  fake_create_and_run_model <- mock(NULL)

  local_mocked_bindings(
    write_civis_file = fake_write_civis_file,
    get_database_id = fake_get_database_id,
    create_and_run_model = fake_create_and_run_model,
    get_train_template_id = function(...) ml_train_template_id
  )
  civis_ml(iris,
           model_type = "sparse_logistic",
           dependent_variable = "the_target_column",
           primary_key = "the_pk_column")

  expect_args(fake_create_and_run_model, 1,
              file_id = 1234,
              dependent_variable = "the_target_column",
              excluded_columns = NULL,
              primary_key = "the_pk_column",
              model_type = "sparse_logistic",
              parameters = NULL,
              cross_validation_parameters = NULL,
              fit_params = NULL,
              calibration = NULL,
              oos_scores_table = NULL,
              oos_scores_db_id = NULL,
              oos_scores_if_exists = 'fail',
              model_name = NULL,
              cpu_requested = NULL,
              memory_requested = NULL,
              disk_requested = NULL,
              validation_data = 'train',
              n_jobs = NULL,
              notifications = NULL,
              verbose = FALSE,
              civisml_version = "prod")
})

test_that("calls civis_ml.civis_table for table_name", {
  fake_get_database_id <- mock(456)
  fake_create_and_run_model <- mock(NULL)

  local_mocked_bindings(
    get_database_id = fake_get_database_id,
    create_and_run_model = fake_create_and_run_model
  )
  x <- civis_table(table_name = "a_schema.table",
                   database_name = "a_database",
                   sql_where = "a = b",
                   sql_limit = 6)

  civis_ml(x = x,
           model_type = "sparse_logistic",
           dependent_variable = "the_target_column",
           primary_key = "the_pk_column")

  expect_args(fake_get_database_id, 1, "a_database")

  expect_args(fake_create_and_run_model, 1,
              table_name = "a_schema.table",
              database_id = 456,
              sql_where = "a = b",
              sql_limit = 6,
              dependent_variable = "the_target_column",
              excluded_columns = NULL,
              primary_key = "the_pk_column",
              model_type = "sparse_logistic",
              parameters = NULL,
              cross_validation_parameters = NULL,
              fit_params = NULL,
              calibration = NULL,
              oos_scores_table = NULL,
              oos_scores_db_id = NULL,
              oos_scores_if_exists = 'fail',
              model_name = NULL,
              cpu_requested = NULL,
              memory_requested = NULL,
              disk_requested = NULL,
              validation_data = 'train',
              n_jobs = NULL,
              notifications = NULL,
              verbose = FALSE,
              civisml_version = "prod")
})

test_that("calls civis_ml.civis_file for file_id", {
  fake_get_database_id <- mock(456)
  fake_create_and_run_model <- mock(NULL)

  local_mocked_bindings(
    get_database_id = fake_get_database_id,
    create_and_run_model = fake_create_and_run_model
  )

  civis_ml(x = civis_file(file_id = 123),
           model_type = "sparse_logistic",
           dependent_variable = "the_target_column",
           primary_key = "the_pk_column")

  expect_args(fake_create_and_run_model, 1,
              file_id = civis_file(123),
              dependent_variable = "the_target_column",
              excluded_columns = NULL,
              primary_key = "the_pk_column",
              model_type = "sparse_logistic",
              parameters = NULL,
              cross_validation_parameters = NULL,
              fit_params = NULL,
              calibration = NULL,
              oos_scores_table = NULL,
              oos_scores_db_id = NULL,
              oos_scores_if_exists = 'fail',
              model_name = NULL,
              cpu_requested = NULL,
              memory_requested = NULL,
              disk_requested = NULL,
              validation_data = 'train',
              n_jobs = NULL,
              notifications = NULL,
              verbose = FALSE,
              civisml_version = "prod")
})

test_that("calls civis_ml.character for local csv", {
  fake_get_database_id <- mock(456)
  fake_write_civis_file <- mock(123)
  fake_create_and_run_model <- mock(NULL)

  local_mocked_bindings(
    get_database_id = fake_get_database_id,
    write_civis_file = fake_write_civis_file,
    create_and_run_model = fake_create_and_run_model
  )

  civis_ml(x =  "fake_temp_path",
           model_type = "sparse_logistic",
           dependent_variable = "the_target_column",
           primary_key = "the_pk_column")

  expect_args(fake_write_civis_file, 1,
              path = "fake_temp_path",
              name = "modelpipeline_data.csv")

  expect_args(fake_create_and_run_model, 1,
              file_id = 123,
              dependent_variable = "the_target_column",
              excluded_columns = NULL,
              primary_key = "the_pk_column",
              model_type = "sparse_logistic",
              parameters = NULL,
              cross_validation_parameters = NULL,
              fit_params = NULL,
              calibration = NULL,
              oos_scores_table = NULL,
              oos_scores_db_id = NULL,
              oos_scores_if_exists = 'fail',
              model_name = NULL,
              cpu_requested = NULL,
              memory_requested = NULL,
              disk_requested = NULL,
              validation_data = 'train',
              n_jobs = NULL,
              notifications = NULL,
              verbose = FALSE,
              civisml_version = "prod")
})

test_that("raises error on invalid calibration", {
  fake_get_database_id <- mock(456)
  fake_write_civis_file <- mock(123)

  local_mocked_bindings(
    get_database_id = fake_get_database_id,
    write_civis_file = fake_write_civis_file
  )

  expect_error(civis_ml(x = "fake_temp_path",
                        model_type = "sparse_logistic",
                        dependent_variable = "target",
                        primary_key = "pk",
                        calibration = "fake"),
               "calibration must be 'sigmoid', 'isotonic', or NULL\\.")
})

test_that("raises error if multioutput not supported", {
  fake_get_database_id <- mock(456, cycle = TRUE)
  fake_write_civis_file <- mock(123, cycle = TRUE)
  mo_not_supported <- c("sparse_linear_regressor", "sparse_ridge_regressor", "gradient_boosting_regressor",
    "sparse_logistic", "gradient_boosting_classifier")

  for (mtype in mo_not_supported) {
    local_mocked_bindings(
      get_database_id = fake_get_database_id,
      write_civis_file = fake_write_civis_file
    )
    expect_error(civis_ml(x = "fake_temp_path",
              model_type = mtype,
              dependent_variable = c("target", "target_2"),
              primary_key = "pk",
              calibration = "fake"),
            paste0("Multioutput is not supported for ", mtype))

  }
})

################################################################################
# Predict
context("predict.civis_ml")

fake_model <- structure(
  list(
    job = list(
      id = 123,
      name = "model_task",
      fromTemplateId = ml_train_template_id,
      arguments = list(
        PRIMARY_KEY = "training_primary_key"
      )
    ),
    run = list(id = 456)
  ),
  class = "civis_ml"
)

test_that("calls scripts_post_custom", {
  fake_get_database_id <- mock(456, cycle = TRUE)
  fake_scripts_post_custom <- mock(list(id = 999))
  fake_scripts_post_custom_runs <- mock(list(id = 888))
  fake_scripts_get_custom_runs <- mock(list(state = "running"), list(state = "succeeded"))
  fake_scripts_get_custom <- mock(list(state = "succeeded"), cycle = TRUE)
  fake_fetch_predict_results <- mock(NULL)

  local_mocked_bindings(
    get_database_id = fake_get_database_id,
    scripts_post_custom = fake_scripts_post_custom,
    scripts_post_custom_runs = fake_scripts_post_custom_runs,
    scripts_get_custom = fake_scripts_get_custom,
    scripts_get_custom_runs = fake_scripts_get_custom_runs,
    fetch_predict_results = fake_fetch_predict_results,
    get_predict_template_id = function(...) ml_predict_template_id
  )

  tbl <- civis_table(table_name = "schema.table",
                     database_name = "a_database",
                     sql_where = "6 = 7",
                     sql_limit = 7)
  predict(fake_model,
          newdata = tbl,
          primary_key = "row_number",
          output_table = "score.table",
          output_db = "score_database",
          if_output_exists = "append",
          n_jobs = 10,
          cpu_requested = 2000,
          memory_requested = 10,
          disk_requested = 15,
          polling_interval = .01,
          verbose = TRUE)

  script_args <- mock_args(fake_scripts_post_custom)[[1]]
  expect_equal(script_args$from_template_id, ml_predict_template_id)
  expect_equal(script_args$name, "model_task Predict")

  # These are template args/params:
  pred_args <- script_args$arguments
  expect_is(pred_args, "AsIs")  # We don't want jsonlite doing anything unexpected.
  expect_equal(pred_args$TRAIN_JOB, 123)
  expect_equal(pred_args$TRAIN_RUN, 456)
  expect_equal(pred_args$PRIMARY_KEY, "row_number")
  expect_equal(pred_args$IF_EXISTS, "append")
  expect_equal(pred_args$N_JOBS, 10)
  expect_equal(pred_args$CPU, 2000)
  expect_equal(pred_args$MEMORY, 10)
  expect_equal(pred_args$DISK_SPACE, 15)
  expect_equal(pred_args$DEBUG, TRUE)
  expect_equal(pred_args$CIVIS_FILE_ID, NULL)
  expect_equal(pred_args$TABLE_NAME, "schema.table")
  expect_equal(pred_args$DB, list(database = 456))
  expect_equal(pred_args$WHERESQL, "6 = 7")
  expect_equal(pred_args$LIMITSQL, 7)
  expect_equal(pred_args$OUTPUT_TABLE, "score.table")
  expect_equal(pred_args$OUTPUT_DB, list(database = 456))

  # Make sure we started the job.
  expect_args(fake_scripts_post_custom_runs, 1, 999)

  # And checked it's status
  expect_args(fake_scripts_get_custom_runs, 1, 999, 888)
  expect_called(fake_scripts_get_custom_runs, 2)
})

test_that("uses training primary_key by default", {
  fake_get_database_id <- mock(123)
  fake_create_and_run_pred <- mock(NULL)

  local_mocked_bindings(
    get_database_id = fake_get_database_id,
    create_and_run_pred = fake_create_and_run_pred,
    get_predict_template_id = function(...) ml_predict_template_id
  )

  tbl <- civis_table(table_name = "schema.table", database_name = "the_db")
  predict(fake_model, newdata = tbl)

  run_args <- mock_args(fake_create_and_run_pred)[[1]]
  expect_equal(run_args$primary_key, "training_primary_key")
})

test_that("uploads local df and passes a file_id", {
  fake_write_civis_file <- mock(1234)
  fake_create_and_run_pred <- mock(NULL)

  local_mocked_bindings(
    write_civis_file = fake_write_civis_file,
    create_and_run_pred = fake_create_and_run_pred,
    get_predict_template_id = function(...) ml_predict_template_id
  )

  predict(fake_model, iris, primary_key = NULL)

  expect_args(fake_create_and_run_pred, 1,
              train_job_id = fake_model$job$id,
              train_run_id = fake_model$run$id,
              template_id = ml_predict_template_id,
              primary_key = NULL,
              output_table = NULL,
              output_db_id = NULL,
              if_output_exists = 'fail',
              model_name = "model_task",
              n_jobs = NULL,
              cpu_requested = NULL,
              memory_requested = NULL,
              disk_requested = NULL,
              polling_interval = NULL,
              verbose = FALSE,
              file_id = 1234)
})

test_that("uploads a local file and passes a file_id", {
  fake_write_civis_file <- mock(561)
  fake_create_and_run_pred <- mock(NULL)

  local_mocked_bindings(
    write_civis_file = fake_write_civis_file,
    create_and_run_pred = fake_create_and_run_pred,
    get_predict_template_id = function(...) ml_predict_template_id
  )

  predict(fake_model, "fake_temp_path", primary_key = NULL)

  expect_args(fake_write_civis_file, 1,
              "fake_temp_path",
              "modelpipeline_data.csv")

  expect_args(fake_create_and_run_pred, 1,
              train_job_id = fake_model$job$id,
              train_run_id = fake_model$run$id,
              template_id = ml_predict_template_id,
              primary_key = NULL,
              output_table = NULL,
              output_db_id = NULL,
              if_output_exists = 'fail',
              model_name = "model_task",
              n_jobs = NULL,
              cpu_requested = NULL,
              memory_requested = NULL,
              disk_requested = NULL,
              polling_interval = NULL,
              verbose = FALSE,
              file_id = 561)
})

test_that("passes a file_id directly", {
  fake_create_and_run_pred <- mock(NULL)

  local_mocked_bindings(
    create_and_run_pred = fake_create_and_run_pred,
    get_predict_template_id = function(...) ml_predict_template_id
  )

  predict(fake_model, civis_file(1234))

  expect_args(fake_create_and_run_pred, 1,
              train_job_id = fake_model$job$id,
              train_run_id = fake_model$run$id,
              template_id = ml_predict_template_id,
              primary_key = "training_primary_key",
              output_table = NULL,
              output_db_id = NULL,
              if_output_exists = 'fail',
              model_name = "model_task",
              n_jobs = NULL,
              cpu_requested = NULL,
              memory_requested = NULL,
              disk_requested = NULL,
              polling_interval = NULL,
              verbose = FALSE,
              file_id = 1234)
})

test_that("passes a manifest file_id", {
  fake_create_and_run_pred <- mock(NULL)

  local_mocked_bindings(
    create_and_run_pred = fake_create_and_run_pred,
    get_predict_template_id = function(...) ml_predict_template_id
  )

  predict(fake_model, civis_file_manifest(123), primary_key = NULL)

  expect_args(fake_create_and_run_pred, 1,
              train_job_id = fake_model$job$id,
              train_run_id = fake_model$run$id,
              template_id = ml_predict_template_id,
              primary_key = NULL,
              output_table = NULL,
              output_db_id = NULL,
              if_output_exists = 'fail',
              model_name = "model_task",
              n_jobs = NULL,
              cpu_requested = NULL,
              memory_requested = NULL,
              disk_requested = NULL,
              polling_interval = NULL,
              verbose = FALSE,
              manifest = 123)
})

test_that("passes table info", {
  fake_get_database_id <- mock(999)
  fake_create_and_run_pred <- mock(NULL)

  local_mocked_bindings(
    get_database_id = fake_get_database_id,
    create_and_run_pred = fake_create_and_run_pred,
    get_predict_template_id = function(...) ml_predict_template_id
  )

  table_to_score <- civis_table(
    table_name = "a_schema.table",
    database_name = "a_database",
    sql_where = "row_number in (1, 2, 4)",
    sql_limit = 11
  )
  predict(fake_model, table_to_score, primary_key = NULL)

  expect_args(fake_get_database_id, 1, "a_database")
  expect_args(fake_create_and_run_pred, 1,
              train_job_id = fake_model$job$id,
              train_run_id = fake_model$run$id,
              template_id = ml_predict_template_id,
              primary_key = NULL,
              output_table = NULL,
              output_db_id = NULL,
              if_output_exists = 'fail',
              model_name = "model_task",
              n_jobs = NULL,
              cpu_requested = NULL,
              memory_requested = NULL,
              disk_requested = NULL,
              polling_interval = NULL,
              verbose = FALSE,
              table_name = "a_schema.table",
              database_id = 999,
              sql_where = "row_number in (1, 2, 4)",
              sql_limit = 11)
})

################################################################################
context("stash_local_dataframe")

test_that("newer CivisML versions use feather", {
  # enforce newer CivisML version
  temp_id <- 11219
  # factor should not cause errors when using feather
  x <- data.frame(a = 1:3, b = letters[1:3])
  fake_file <- mock(1)
  local_mocked_bindings(
    write_civis_file = fake_file
  )
  stash_local_dataframe(x, temp_id)
  args <- mock_args(fake_file)
  expect_equal(args[[1]]$name, "modelpipeline_data.feather")

})

test_that("older CivisML versions use csv", {
  # enforce older CivisML version
  temp_id <- 9969
  # factor type should not matter for older version
  x <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  fake_file <- mock(1)
  local_mocked_bindings(
    write_civis_file = fake_file
  )
  stash_local_dataframe(x, temp_id)
  args <- mock_args(fake_file)
  expect_equal(args[[1]]$name, "modelpipeline_data.csv")
})

################################################################################
# run build model
context("create_and_run_model")

test_that("uses the correct template_id", {
  fake_run_model <- mock(list(job_id = 133, run_id = 244))
  fake_civis_ml_fetch_existing <- mock(NULL)

  local_mocked_bindings(
    run_model = fake_run_model,
    civis_ml_fetch_existing = fake_civis_ml_fetch_existing,
    get_train_template_id = function(...) ml_train_template_id
  )

  create_and_run_model(file_id = 123)

  run_args <- mock_args(fake_run_model)[[1]]
  expect_equal(run_args$template_id, ml_train_template_id)
})

test_that("converts parameters arg to JSON string", {
  fake_run_model <- mock(list(job_id = 133, run_id = 244))
  fake_civis_ml_fetch_existing <- mock(NULL)

  local_mocked_bindings(
    run_model = fake_run_model,
    civis_ml_fetch_existing = fake_civis_ml_fetch_existing,
    get_train_template_id = function(...) ml_train_template_id
  )

  create_and_run_model(file_id = 123, parameters = list(n_trees = 500, c = -1))

  run_args <- mock_args(fake_run_model)[[1]]
  expect_equal(unclass(run_args$arguments$PARAMS), '{"n_trees":500,"c":-1}')
})

test_that("converts cross_validation_parameters to JSON string", {
  fake_run_model <- mock(list(job_id = 133, run_id = 244))
  fake_civis_ml_fetch_existing <- mock(NULL)

  local_mocked_bindings(
    run_model = fake_run_model,
    civis_ml_fetch_existing = fake_civis_ml_fetch_existing,
    get_train_template_id = function(...) ml_train_template_id
  )

  create_and_run_model(file_id = 123,
                        model_type = "sparse_logistic",
                        cross_validation_parameters = list(n_trees = c(500, 250), c = -1))

  run_args <- mock_args(fake_run_model)[[1]]
  expect_equal(unclass(run_args$arguments$CVPARAMS),
               '{"n_trees":[500,250],"c":[-1]}')
})

test_that("converts fit_params to JSON string", {
  fake_run_model <- mock(list(job_id = 133, run_id = 244))
  fake_civis_ml_fetch_existing <- mock(NULL)

  local_mocked_bindings(
    run_model = fake_run_model,
    civis_ml_fetch_existing = fake_civis_ml_fetch_existing,
    get_train_template_id = function(...) ml_train_template_id
  )

  create_and_run_model(file_id = 123,
                       fit_params = list(weights = "weight_col"))

  run_args <- mock_args(fake_run_model)[[1]]
  expect_equal(unclass(run_args$arguments$FIT_PARAMS), '{"weights":"weight_col"}')
})

test_that("space separates excluded_columns", {
  fake_run_model <- mock(list(job_id = 133, run_id = 244))
  fake_civis_ml_fetch_existing <- mock(NULL)

  local_mocked_bindings(
    run_model = fake_run_model,
    civis_ml_fetch_existing = fake_civis_ml_fetch_existing,
    get_train_template_id = function(...) ml_train_template_id
  )

  create_and_run_model(file_id = 132, excluded_columns = c("c1", "c2", "c3"))

  run_args <- mock_args(fake_run_model)[[1]]
  expect_equal(run_args$arguments$EXCLUDE_COLS, "c1 c2 c3")
})

test_that("space separates target_column", {
  fake_run_model <- mock(list(job_id = 133, run_id = 244))
  fake_civis_ml_fetch_existing <- mock(NULL)

  local_mocked_bindings(
    run_model = fake_run_model,
    civis_ml_fetch_existing = fake_civis_ml_fetch_existing,
    get_train_template_id = function(...) ml_train_template_id
  )

  create_and_run_model(file_id = 132, dependent_variable = c("c1", "c2"),
                       model_type = "random_forest_regressor")

  run_args <- mock_args(fake_run_model)[[1]]
  expect_equal(run_args$arguments$TARGET_COLUMN, "c1 c2")
})

test_that("file_id is always numeric", {
  fake_run_model <- mock(list(job_id = 133, run_id = 244))
  fake_civis_ml_fetch_existing <- mock(NULL)

  local_mocked_bindings(
    run_model = fake_run_model,
    civis_ml_fetch_existing = fake_civis_ml_fetch_existing,
    get_train_template_id = function(...) ml_train_template_id
  )

  create_and_run_model(file_id = civis_file(132))

  run_args <- mock_args(fake_run_model)[[1]]
  expect_equal(run_args$arguments$CIVIS_FILE_ID, 132)
})

test_that("exceptions with hyperband correct", {
  fake_run_model <- mock(list(job_id = 133, run_id = 244))
  fake_civis_ml_fetch_existing <- mock(NULL)

  local_mocked_bindings(
    run_model = fake_run_model,
    civis_ml_fetch_existing = fake_civis_ml_fetch_existing
  )
  err <- "cross_validation_parameters = \"hyperband\" not supported for sparse_logistic"
  expect_error(create_and_run_model(file_id = civis_file(132),
                                    model_type = "sparse_logistic",
                                    cross_validation_parameters = "hyperband"), err)
})

test_that("robust if metrics.json not present", {
  fn <- tempfile()
  cat(jsonlite::toJSON(list(a=1, b=letters[1:3])), file = fn)

  fake_outputs <- mock(list(list(objectType = "File", objectId = 1, name = "model_info.json")))
  fake_download <- mock(fn)
  fake_fetch_job <- mock(1)
  fake_fetch_run <- mock(list(state = "succeeded"))
  fake_model_type <- mock("regressor")
  local_mocked_bindings(
    must_fetch_civis_ml_job = fake_fetch_job,
    must_fetch_civis_ml_run = fake_fetch_run,
    scripts_list_custom_runs_outputs = fake_outputs,
    download_civis = fake_download,
    model_type = fake_model_type
  )
  res <- civis_ml_fetch_existing(123, 1)
  expect_equal(res$model_info, list(a=1, b=letters[1:3]))
  expect_null(res$metrics)
})



################################################################################
# run predictions
context("create_and_run_pred")

test_that("uses the correct template_id", {
  fake_run_model <- mock(list(job_id = 133, run_id = 244))
  fake_fetch_predict_results <- mock(NULL)

  local_mocked_bindings(
    run_model = fake_run_model,
    fetch_predict_results = fake_fetch_predict_results
  )

  create_and_run_pred(train_job_id = 111, train_run_id = 222, template_id = 555)
  run_args <- mock_args(fake_run_model)[[1]]
  expect_equal(run_args$template_id, 555)
})

################################################################################
# fetch existing model
context("civis_ml_fetch_existing")

test_that("raises an error on not found", {
  fake_scripts_get_custom <- function(id) stop(httr::http_condition(404L, "error"))

  local_mocked_bindings(
    scripts_get_custom = fake_scripts_get_custom
  )
  expect_error(civis_ml_fetch_existing(123), "Error: model 123 not found\\.")
})

test_that("raises an error on invalid model", {
  fake_must_fetch_civis_ml_job <- mock(
    list(
      lastRun = list(
        id = NULL
      )
    )
  )
  local_mocked_bindings(
    must_fetch_civis_ml_job = fake_must_fetch_civis_ml_job
  )
  expect_error(civis_ml_fetch_existing(123), "Error: invalid model task\\.")
})

test_that("issues message for still running", {
  fake_must_fetch_civis_ml_job <- mock(
    list(
      lastRun = list(
        id = 456
      ),
      arguments = list(
          MODEL = "regressor"
        )
    )
  )
  fake_scripts_get_custom_runs <- mock(list(state = "running"))

  local_mocked_bindings(
    must_fetch_civis_ml_job = fake_must_fetch_civis_ml_job,
    scripts_get_custom_runs = fake_scripts_get_custom_runs
  )
  expect_message(civis_ml_fetch_existing(123),
                 "The model task is still running\\.")
})

test_that("raises an error if job failed", {
  fake_must_fetch_civis_ml_job <- mock(
    list(
      lastRun = list(
        id = 456
      ),
      arguments = list(
          MODEL = "regressor"
        )
      )
  )
  fake_scripts_get_custom_runs <- mock(list(state = "failed"))

  local_mocked_bindings(
    must_fetch_civis_ml_job = fake_must_fetch_civis_ml_job,
    scripts_get_custom_runs = fake_scripts_get_custom_runs
  )
  expect_warning(civis_ml_fetch_existing(123),
                 "The model task failed, use fetch_logs to retreive any error messages.")
})

test_that("fetch_logs.civis_ml_error works", {
  local_mocked_bindings(
    scripts_post_custom = function(...) NULL,
    scripts_post_custom_runs = function(...) NULL,
    scripts_get_custom_runs = function(...) list(state = "failed", id = 1, run_id = 2, error = "msg"),
    fetch_logs.civis_ml_error = function(...) list("A log message")
  )
  e <- tryCatch(civis:::run_model(1234, name = "sparse_logistic", list(), list(),
                                  verbose = TRUE, polling_interval = NULL),
            error = function(e) e)
  log <- fetch_logs(e)[[1]]
  expect_equal(log, "A log message")
})

###############################################################################
context("run_model")

test_that("it removes notifications when NULL", {
  fake_scripts_post_custom <- mock(list(id = 123))
  fake_scripts_post_custom_runs <- mock(list(id = 456))
  fake_scripts_get_custom_runs <- mock(list(state = "succeeded"))

  local_mocked_bindings(
    scripts_post_custom = fake_scripts_post_custom,
    scripts_post_custom_runs = fake_scripts_post_custom_runs,
    scripts_get_custom_runs = fake_scripts_get_custom_runs
  )
  run_model(template_id = 123, name = "a name", arguments = list(a = "b"),
            notifications = NULL, verbose = TRUE)
  script_args <- mock_args(fake_scripts_post_custom)[[1]]
  expect_false("notifications" %in% names(script_args))
})

test_that("it removes name when NULL", {
  fake_scripts_post_custom <- mock(list(id = 123))
  fake_scripts_post_custom_runs <- mock(list(id = 456))
  fake_scripts_get_custom_runs <- mock(list(state = "succeeded", NULL))

  local_mocked_bindings(
    scripts_post_custom = fake_scripts_post_custom,
    scripts_post_custom_runs = fake_scripts_post_custom_runs,
    scripts_get_custom_runs = fake_scripts_get_custom_runs
  )
  run_model(template_id = 123, name = NULL, arguments = list(a = "b"),
            notifications = NULL, verbose = TRUE)
  script_args <- mock_args(fake_scripts_post_custom)[[1]]
  expect_false("name" %in% names(script_args))
})

test_that("civis_ml_error is caught from run_model", {
  local_mocked_bindings(
    scripts_post_custom = function(...) NULL,
    scripts_post_custom_runs = function(...) NULL,
    scripts_get_custom_runs = function(...) list(state = "failed", id = 1, run_id = 2, error = "msg"),
    fetch_logs.civis_ml_error = function(...) list("A log message")
  )
  e <- tryCatch(civis:::run_model(1234, name = "sparse_logistic", list(), list(), verbose = TRUE),
                error = function(e) e)
  msg <- "scripts_get_custom_runs(... = NULL, run_id = NULL): msg\nA log message"
  expect_equal(e$message, msg)
  expect_is(e, c("civis_ml_error", "civis_error"))
  expect_true(any(grepl("A log message", capture.output(print(e)))))

  err_data <- get_error(e)
  expect_equal(err_data$f, "scripts_get_custom_runs")
  expect_equal(err_data$log[[1]], "A log message")
})

###############################################################################
context("fetch_oos_scores")

test_that("it checks input type", {
  fake_must_fetch_output_file <- mock(NULL)

  local_mocked_bindings(
    must_fetch_output_file = fake_must_fetch_output_file
  )
  expect_error(fetch_oos_scores("not a model"), "is_civis_ml(model) is not TRUE", fixed = TRUE)
})

test_that("it looks for predictions.csv.gz", {
  fake_must_fetch_output_file <- mock(textConnection(c("a, b, c")))

  local_mocked_bindings(
    must_fetch_output_file = fake_must_fetch_output_file
  )
  fetch_oos_scores(structure(list(), class = "civis_ml"))
  fetch_args <- mock_args(fake_must_fetch_output_file)[[1]]
  expect_equal(fetch_args[[2]], "predictions.csv.gz")
})

test_that("it calls read.csv with extra args", {
  fake_must_fetch_output_file <- mock(textConnection(c("a,b,c")))

  local_mocked_bindings(
    must_fetch_output_file = fake_must_fetch_output_file
  )
  df <- fetch_oos_scores(structure(list(), class = "civis_ml"),
                         stringsAsFactors = FALSE, header = FALSE)
  ans <- data.frame(V1 = "a", V2 = "b", V3 = "c", stringsAsFactors = FALSE)
  expect_equal(df, ans)
})


###############################################################################
context("fetch_predictions")

test_that("it checks input type", {
  expect_error(fetch_predictions("not a model"), "is(x, \"civis_ml_prediction\") is not TRUE", fixed = TRUE)
})

test_that("it calls read.csv with extra args, and dowload_civis with correct id", {
  fake_read_csv <- mock(NULL)
  fake_download_civis <- mock(textConnection(c("a,b,c")))

  local_mocked_bindings(
    fetch_predict_results = function(...) list(model_info = list(output_file_ids = 1)),
    download_civis = fake_download_civis
  )
  df <- fetch_predictions(structure(list(), class = "civis_ml_prediction"),
                      header = FALSE, stringsAsFactors = FALSE)
  ans <- data.frame(V1 = "a", V2 = "b", V3 = "c", stringsAsFactors = FALSE)
  expect_equal(df, ans)

  dl_args <- mock_args(fake_download_civis)[[1]]
  expect_equal(dl_args[[1]], 1)
})
