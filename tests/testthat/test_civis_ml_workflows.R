library(civis)
library(mockery)

source("utils.R")

################################################################################
context("civis_ml_sparse_logistic")

test_that("calls civis_ml with model_type", {
  check_civis_ml_model_type(civis_ml_sparse_logistic,
                            "sparse_logistic")
})

test_that("calls civis_ml with params", {
  fake_civis_ml <- mock(NULL)

  local_mocked_bindings(
    civis_ml = fake_civis_ml
  )
  civis_ml_sparse_logistic(iris,
                           dependent_variable = "Species",
                           C = 9,
                           fit_intercept = FALSE,
                           class_weight = "balanced",
                           solver = "lbfgs")

  ml_args <- mock_args(fake_civis_ml)[[1]]
  params <- ml_args$parameters

  expect_equal(params$C, 9)
  expect_equal(params$fit_intercept, FALSE)
  expect_equal(params$class_weight, "balanced")
  expect_equal(params$solver, "lbfgs")
})

test_that("checks class_weight param", {
  expect_error(civis_ml_sparse_logistic(iris, dependent_variable = "Species",
                                        class_weight = "unbalanced"))
  expect_error(civis_ml_sparse_logistic(iris, dependent_variable = "Species",
                                        class_weight = c(1, 2, 3)))
})

test_that("checks solver and penalty", {
  non_l1_solvers <- c("lbfgs", "newton-cg", "sag")
  for (solver in non_l1_solvers) {
    expected_message <- paste0("The l1 penalty is not supported for ", solver, "\\.")
    expect_error(
      civis_ml_sparse_logistic(iris, dependent_variable = "Species",
                               solver = solver, penalty = "l1"),
      expected_message)
  }
})

test_that("checks solver and multinomial loss", {
  non_multinomial_solvers <- c("liblinear")
  for (solver in non_multinomial_solvers) {
    expected_message <- paste0(solver, " does not support multinomial loss\\.")
    expect_error(
      civis_ml_sparse_logistic(iris, dependent_variable = "Species",
                               solver = solver, multi_class = "multinomial"),
      expected_message)
  }
})

test_that("checks solver and dual", {
  non_dual_solvers <- c("newton-cg", "sag", "lbfgs")
  for (solver in non_dual_solvers) {
    expected_message <- paste0(solver, " does not support the dual formulation\\.")
    expect_error(
      civis_ml_sparse_logistic(iris, dependent_variable = "Species",
                               solver = solver, dual = TRUE),
      expected_message)
  }
})

test_that("passes other args to civis_ml", {
  check_civis_ml_call(civis_ml_sparse_logistic)
})

################################################################################
context("civis_ml_sparse_linear_regressor")

test_that("calls civis_ml with model_type", {
  check_civis_ml_model_type(civis_ml_sparse_linear_regressor,
                            "sparse_linear_regressor")
})

test_that("calls civis_ml with params", {
  fake_civis_ml <- mock(NULL)

  local_mocked_bindings(
    civis_ml = fake_civis_ml
  )
  civis_ml_sparse_linear_regressor(x = iris,
                                    dependent_variable = "Sepal.Length",
                                    fit_intercept = TRUE,
                                    normalize = FALSE)
  ml_args <- mock_args(fake_civis_ml)[[1]]
  params <- ml_args$parameters

  expect_equal(params$fit_intercept, TRUE)
  expect_equal(params$normalize, FALSE)
})

test_that("warns when fit_intercept = FALSE and normalize = TRUE", {
  fake_civis_ml <- mock(NULL)

  local_mocked_bindings(
    civis_ml = fake_civis_ml
  )

  expect_warning(
    civis_ml_sparse_linear_regressor(x = iris,
                                      dependent_variable = "Sepal.Length",
                                      fit_intercept = FALSE,
                                      normalize = TRUE),
    "fit_intercept = FALSE, ignoring normalize\\.")
})

test_that("passes other args to civis_ml", {
  check_civis_ml_call(civis_ml_sparse_linear_regressor, can_calibrate = FALSE)
})

################################################################################
context("civis_ml_sparse_ridge_regressor")

test_that("calls civis_ml with model_type", {
  check_civis_ml_model_type(civis_ml_sparse_ridge_regressor,
                            "sparse_ridge_regressor")
})

test_that("passes other args to civis_ml", {
  check_civis_ml_call(civis_ml_sparse_ridge_regressor, can_calibrate = FALSE)
})
test_that("calls civis_ml with params", {
  fake_civis_ml <- mock(NULL)

  local_mocked_bindings(
    civis_ml = fake_civis_ml
  )
  civis_ml_sparse_ridge_regressor(x = iris,
                                  dependent_variable = "Sepal.Length",
                                  alpha = 10.5,
                                  fit_intercept = FALSE,
                                  normalize = TRUE,
                                  max_iter = 9,
                                  tol = 1e-8,
                                  solver = "svd",
                                  random_state = 561)

  ml_args <- mock_args(fake_civis_ml)[[1]]
  params <- ml_args$parameters

  expect_equal(params$alpha, 10.5)
  expect_equal(params$fit_intercept, FALSE)
  expect_equal(params$normalize, TRUE)
  expect_equal(params$max_iter, 9)
  expect_equal(params$tol, 1e-8)
  expect_equal(params$solver, "svd")
  expect_equal(params$random_state, 561)
})

################################################################################
context("civis_ml_gradient_boosting_classifier")

test_that("calls civis_ml with model_type", {
  check_civis_ml_model_type(civis_ml_gradient_boosting_classifier,
                            "gradient_boosting_classifier")
})

test_that("passes other args to civis_ml", {
  check_civis_ml_call(civis_ml_gradient_boosting_classifier)
})

test_that("calls civis_ml with params", {
  fake_civis_ml <- mock(NULL)

  local_mocked_bindings(
    civis_ml = fake_civis_ml
  )
  civis_ml_gradient_boosting_classifier(x = iris,
                                        dependent_variable = "Species",
                                        loss = "deviance",
                                        learning_rate = 0.15,
                                        n_estimators = 100,
                                        subsample = 0.9,
                                        criterion = "mse")

  ml_args <- mock_args(fake_civis_ml)[[1]]
  params <- ml_args$parameters

  expect_equal(params$loss, "deviance")
  expect_equal(params$learning_rate, 0.15)
  expect_equal(params$n_estimators, 100)
  expect_equal(params$subsample, 0.9)
  expect_equal(params$criterion, "mse")
  expect_equal(params$min_samples_split, 2)
  expect_equal(params$min_samples_leaf, 1)
  expect_equal(params$max_depth, 2)
  expect_equal(params$min_impurity_split, 1e-7)
  expect_equal(params$random_state, 42)
  expect_equal(params$max_features, "sqrt")
  expect_equal(params$max_leaf_nodes, NULL)
  expect_equal(params$presort, "auto")
})

################################################################################
context("civis_ml_sparse_ridge_regressor")

test_that("calls civis_ml with model_type", {
  check_civis_ml_model_type(civis_ml_sparse_ridge_regressor,
                            "sparse_ridge_regressor")
})

test_that("passes other args to civis_ml", {
  check_civis_ml_call(civis_ml_sparse_ridge_regressor, can_calibrate = FALSE)
})

test_that("calls civis_ml with params", {
  fake_civis_ml <- mock(NULL)

  local_mocked_bindings(
    civis_ml = fake_civis_ml
  )

  civis_ml_sparse_ridge_regressor(x = iris,
                                  dependent_variable = "Sepal.Length",
                                  alpha = 10.5,
                                  fit_intercept = FALSE,
                                  normalize = TRUE,
                                  max_iter = 9,
                                  tol = 1e-8,
                                  solver = "svd",
                                  random_state = 561)

  ml_args <- mock_args(fake_civis_ml)[[1]]
  params <- ml_args$parameters

  expect_equal(params$alpha, 10.5)
  expect_equal(params$fit_intercept, FALSE)
  expect_equal(params$normalize, TRUE)
  expect_equal(params$max_iter, 9)
  expect_equal(params$tol, 1e-8)
  expect_equal(params$solver, "svd")
  expect_equal(params$random_state, 561)
})

################################################################################
context("civis_ml_random_forest_classifier")

test_that("calls civis_ml with model_type", {
  check_civis_ml_model_type(civis_ml_random_forest_classifier,
                            "random_forest_classifier")
})

test_that("passes other args to civis_ml", {
  check_civis_ml_call(civis_ml_random_forest_classifier)
})

test_that("checks class_weight param", {
  expect_error(civis_ml_random_forest_classifier(iris, dependent_variable = "Species",
                                                 class_weight = "unbalanced"))
  expect_error(civis_ml_random_forest_classifier(iris, dependent_variable = "Species",
                                                 class_weight = c(1, 2, 3)))
})

test_that("calls civis_ml with params", {
  fake_civis_ml <- mock(NULL)

  local_mocked_bindings(
    civis_ml = fake_civis_ml
  )
  civis_ml_random_forest_classifier(x = iris,
                                    dependent_variable = "Species",
                                    criterion = "entropy",
                                    n_estimators = 100)

  ml_args <- mock_args(fake_civis_ml)[[1]]
  params <- ml_args$parameters

  expect_equal(params$n_estimators, 100)
  expect_equal(params$criterion, "entropy")
  expect_equal(params$min_samples_split, 2)
  expect_equal(params$min_samples_leaf, 1)
  expect_equal(params$max_depth, NULL)
  expect_equal(params$min_impurity_split, 1e-7)
  expect_equal(params$random_state, 42)
  expect_equal(params$max_features, "sqrt")
  expect_equal(params$max_leaf_nodes, NULL)
  expect_equal(params$bootstrap, TRUE)
})

################################################################################
context("civis_ml_random_forest_regressor")

test_that("calls civis_ml with model_type", {
  check_civis_ml_model_type(civis_ml_random_forest_regressor,
                            "random_forest_regressor")
})

test_that("passes other args to civis_ml", {
  check_civis_ml_call(civis_ml_random_forest_regressor, can_calibrate = FALSE)
})

test_that("calls civis_ml with params", {
  fake_civis_ml <- mock(NULL)

  local_mocked_bindings(
    civis_ml = fake_civis_ml
  )
  civis_ml_random_forest_regressor(x = iris,
                                    dependent_variable = "Species",
                                    criterion = "mae",
                                    n_estimators = 100)
  ml_args <- mock_args(fake_civis_ml)[[1]]
  params <- ml_args$parameters

  expect_equal(params$n_estimators, 100)
  expect_equal(params$criterion, "mae")
  expect_equal(params$min_samples_split, 2)
  expect_equal(params$min_samples_leaf, 1)
  expect_equal(params$max_depth, NULL)
  expect_equal(params$min_impurity_split, 1e-7)
  expect_equal(params$random_state, 42)
  expect_equal(params$max_features, "sqrt")
  expect_equal(params$max_leaf_nodes, NULL)
  expect_equal(params$bootstrap, TRUE)
})

################################################################################
context("civis_ml_extra_trees_classifier")

test_that("calls civis_ml with model_type", {
  check_civis_ml_model_type(civis_ml_extra_trees_classifier,
                            "extra_trees_classifier")
})

test_that("passes other args to civis_ml", {
  check_civis_ml_call(civis_ml_extra_trees_classifier)
})

test_that("checks class_weight param", {
  expect_error(civis_ml_extra_trees_classifier(iris, dependent_variable = "Species",
                                               class_weight = "unbalanced"))
  expect_error(civis_ml_extra_trees_classifier(iris, dependent_variable = "Species",
                                               class_weight = c(1, 2, 3)))
})

test_that("calls civis_ml with params", {
  fake_civis_ml <- mock(NULL)

  local_mocked_bindings(
    civis_ml = fake_civis_ml
  )
  civis_ml_extra_trees_classifier(x = iris,
                                  dependent_variable = "Species",
                                  criterion = "entropy",
                                  n_estimators = 100)
  ml_args <- mock_args(fake_civis_ml)[[1]]
  params <- ml_args$parameters

  expect_equal(params$n_estimators, 100)
  expect_equal(params$criterion, "entropy")
  expect_equal(params$min_samples_split, 2)
  expect_equal(params$min_samples_leaf, 1)
  expect_equal(params$max_depth, NULL)
  expect_equal(params$min_impurity_split, 1e-7)
  expect_equal(params$random_state, 42)
  expect_equal(params$max_features, "sqrt")
  expect_equal(params$max_leaf_nodes, NULL)
  expect_equal(params$bootstrap, FALSE)
})

################################################################################
context("civis_ml_extra_trees_regressor")

test_that("calls civis_ml with model_type", {
  check_civis_ml_model_type(civis_ml_extra_trees_regressor,
                            "extra_trees_regressor")
})

test_that("passes other args to civis_ml", {
  check_civis_ml_call(civis_ml_extra_trees_regressor, can_calibrate = FALSE)
})

test_that("calls civis_ml with params", {
  fake_civis_ml <- mock(NULL)

  local_mocked_bindings(
    civis_ml = fake_civis_ml
  )
  civis_ml_extra_trees_regressor(x = iris,
                                  dependent_variable = "Species",
                                  criterion = "mae",
                                  n_estimators = 100)
  ml_args <- mock_args(fake_civis_ml)[[1]]
  params <- ml_args$parameters

  expect_equal(params$n_estimators, 100)
  expect_equal(params$criterion, "mae")
  expect_equal(params$min_samples_split, 2)
  expect_equal(params$min_samples_leaf, 1)
  expect_equal(params$max_depth, NULL)
  expect_equal(params$min_impurity_split, 1e-7)
  expect_equal(params$random_state, 42)
  expect_equal(params$max_features, "sqrt")
  expect_equal(params$max_leaf_nodes, NULL)
  expect_equal(params$bootstrap, FALSE)
})
