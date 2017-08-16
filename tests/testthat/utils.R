check_civis_ml_call <- function(workflow_fn, can_calibrate = TRUE) {
  fake_civis_ml <- mock(NULL)

  args <- list(x = iris,
               dependent_variable = "Species",
               excluded_columns = c("col_1", "col_2", "col_3"),
               primary_key = "row_number",
               cross_validation_parameters = list(n_estimators = c(10, 20, 30)),
               model_name = "awesome civisml",
               oos_scores_table = "score.table",
               oos_scores_db = "another_database",
               oos_scores_if_exists = "drop",
               fit_params = list(sample_weight = "survey_weights"),
               cpu_requested = 1111,
               memory_requested = 9096,
               disk_requested = 9,
               notifications = list(successEmailSubject = "A success",
                                   successEmailAddresses = c("user@example.com")),
               polling_interval = 5,
               verbose = FALSE)
  if (can_calibrate) {
    args[["calibration"]] <- "sigmoid"
  }

  with_mock(
    `civis::civis_ml` = fake_civis_ml,
     do.call(workflow_fn, args)
  )

  ml_args <- mock_args(fake_civis_ml)[[1]]
  expect_equal(ml_args$x, iris)
  expect_equal(ml_args$dependent_variable, "Species")
  expect_equal(ml_args$primary_key, "row_number")
  expect_equal(ml_args$cross_validation_parameters, list(n_estimators = c(10, 20, 30)))
  expect_equal(ml_args$model_name, "awesome civisml")
  expect_equal(ml_args$oos_scores_table, "score.table")
  expect_equal(ml_args$oos_scores_db, "another_database")
  expect_equal(ml_args$oos_scores_if_exists, "drop")
  expect_equal(ml_args$fit_params, list(sample_weight = "survey_weights"))
  expect_equal(ml_args$cpu_requested, 1111)
  expect_equal(ml_args$memory_requested, 9096)
  expect_equal(ml_args$disk_requested, 9)
  expect_equal(ml_args$notifications, list(successEmailSubject = "A success",
                                           successEmailAddresses = c("user@example.com")))
  expect_equal(ml_args$polling_interval, 5)
  expect_equal(ml_args$verbose, FALSE)

  if (can_calibrate) {
    expect_equal(ml_args$calibration, "sigmoid")
  }
}

check_civis_ml_model_type <- function(workflow_fn, workflow_name) {
  fake_civis_ml <- mock(NULL)

  with_mock(
    `civis::civis_ml` = fake_civis_ml,

    workflow_fn(iris, dependent_variable = "Specis")
  )

  ml_args <- mock_args(fake_civis_ml)[[1]]
  expect_equal(ml_args$model_type, workflow_name)
}
