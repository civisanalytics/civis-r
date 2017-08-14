#' CivisML Sparse Logistic
#'
#' @inheritParams civis_ml
#' @param penalty Used to specify the norm used in the penalization. The
#'   \code{newton-cg}, \code{sag}, and \code{lbfgs} solvers support only l2
#'   penalties.
#' @param dual Dual or primal formulation. Dual formulation is only implemented
#'   for \code{l2} penalty with the \code{liblinear} solver. \code{dual = FALSE}
#'   should be preferred when n_samples > n_features.
#' @param tol Tolerance for stopping criteria.
#' @param C Inverse of regularization strength, must be a positive float.
#'   Smaller values specify stronger regularization.
#' @param fit_intercept Should a constant or intercept term be included in the
#'   model.
#' @param intercept_scaling Useful only when the \code{solver = "liblinear"}
#'   and \code{fit_intercept = TRUE}. In this case, a constant term with the
#'   value \code{intercept_scaling} is added to the design matrix.
#' @param class_weight A \code{list} with \code{class_label = value} pairs, or
#'   \code{balanced}. When \code{class_weight = "balanced"}, the class weights
#'   will be inversely proportional to the class frequencies in the input data
#'   as:
#'     \deqn{ \frac{n_samples}{n_classes * table(y)} }
#'
#'   Note, the class weights are multiplied with \code{sample_weight}
#'   (passed via \code{fit_params}) if \code{sample_weight} is specified.
#' @param random_state The seed of the random number generator to use when
#'   shuffling the data. Used only in \code{solver = "sag"} and
#'   \code{solver = "liblinear"}.
#' @param solver Algorithm to use in the optimization problem. For small data
#'   \code{liblinear} is a good choice. \code{sag} is faster for larger
#'   problems. For multiclass problems, only \code{newton-cg}, \code{sag}, and
#'   \code{lbfgs} handle multinomial loss. \code{liblinear} is limited to
#'   one-versus-rest schemes. \code{newton-cg}, \code{lbfgs}, and \code{sag}
#'   only handle the \code{l2} penalty.
#'
#'   Note that \code{sag} fast convergence is only guaranteed on features with
#'   approximately the same scale.
#' @param max_iter The maximum number of iterations taken for the solvers to
#'   converge. Useful for the \code{newton-cg}, \code{sag}, and \code{lbfgs}
#'   solvers.
#' @param multi_class The scheme for multi-class problems. When \code{ovr}, then
#'   a binary problem is fit for each label. When \code{multinomial}, a single
#'   model is fit minimizing the multinomial loss. Note, \code{multinomial} only
#'   works with the \code{newton-cg}, \code{sag}, and \code{lbfgs} solvers.
#'
#' @inheritSection civis_ml Data Sources
#' @inherit civis_ml return
#' @examples \dontrun{
#'
#' df <- iris
#' names(df) <- stringr::str_replace(names(df), "\\.", "_")
#'
#' m <- civis_ml_sparse_logistic(df, "Species")
#' yhat <- fetch_oos_scores(m)
#'
#' # Grid Search
#' cv_params <- list(C = c(.01, 1, 10, 100, 1000))
#'
#' m <- civis_ml_sparse_logistic(df, "Species",
#'   cross_validation_parameters = cv_params)
#'
#' # make a prediction job, storing in a redshift table
#' pred_info <- predict(m, newdata = civis_table("schema.table", "my_database"),
#'    output_table = "schema.scores_table")
#'
#' }
#' @export
civis_ml_sparse_logistic <- function(x,
                                     dependent_variable,
                                     primary_key = NULL,
                                     excluded_columns = NULL,
                                     penalty = c("l2", "l1"),
                                     dual = FALSE,
                                     tol = 1e-8,
                                     C = 499999950,
                                     fit_intercept = TRUE,
                                     intercept_scaling = 1,
                                     class_weight = NULL,
                                     random_state = 42,
                                     solver = c("liblinear", "newton-cg", "lbfgs", "sag"),
                                     max_iter = 100,
                                     multi_class = c("ovr", "multinomial"),
                                     fit_params = NULL,
                                     cross_validation_parameters = NULL,
                                     calibration = c(NULL, "sigmoid", "isotonic"),
                                     oos_scores_table = NULL,
                                     oos_scores_db = NULL,
                                     oos_scores_if_exists = c("fail", "append", "drop", "truncate"),
                                     model_name = NULL,
                                     cpu_requested = NULL,
                                     memory_requested = NULL,
                                     disk_requested = NULL,
                                     notifications = NULL,
                                     polling_interval = NULL,
                                     verbose = FALSE) {

  model_type <- "sparse_logistic"
  penalty <- match.arg(penalty)
  solver <- match.arg(solver)
  multi_class <- match.arg(multi_class)

  params  <- list(
    penalty = penalty,
    dual = dual,
    tol = tol,
    C = C,
    fit_intercept = fit_intercept,
    intercept_scaling = intercept_scaling,
    random_state = random_state,
    solver = solver,
    max_iter = max_iter,
    multi_class = multi_class
  )

  if (!is.null(class_weight)) {
    if (is.character(class_weight)) {
      stopifnot(class_weight == "balanced")
    } else {
      stopifnot(is.list(class_weight))
    }
    params$class_weight <- class_weight
  }

  l1_solvers <- c("liblinear")
  if (penalty == "l1" & !(solver %in% l1_solvers)) {
    stop(paste0("The l1 penalty is not supported for ", solver, "."))
  }

  multinomial_solvers <- c("newton-cg", "sag", "lbfgs")
  if (multi_class == "multinomial" & !(solver %in% multinomial_solvers)) {
    stop(paste0(solver, " does not support multinomial loss."))
  }

  dual_solvers <- c("liblinear")
  if (dual & !(solver %in% dual_solvers)) {
    stop(paste0(solver, " does not support the dual formulation."))
  }

  civis_ml(x = x, dependent_variable = dependent_variable,
           model_type = model_type, primary_key = primary_key,
           excluded_columns = excluded_columns, parameters = params,
           fit_params = fit_params,
           cross_validation_parameters = cross_validation_parameters,
           calibration = calibration, oos_scores_table = oos_scores_table,
           oos_scores_db = oos_scores_db,
           oos_scores_if_exists = oos_scores_if_exists, model_name = model_name,
           cpu_requested = cpu_requested, memory_requested = memory_requested,
           disk_requested = disk_requested, notifications = notifications,
           polling_interval = polling_interval, verbose = verbose)
}

#' CivisML Sparse Linear Regression
#'
#' @inheritParams civis_ml
#' @param fit_intercept Should an intercept term be included in the model. If
#'   \code{FALSE}, no intercept will be included, in this case the data are
#'   expected to already be centered.
#' @param normalize If \code{TRUE}, the regressors will be normalized before
#'   fitting the model. \code{normalize} is ignored when
#'   \code{fit_intercept = FALSE}.
#'
#' @inheritSection civis_ml Data Sources
#' @inherit civis_ml return
#' @examples
#' \dontrun{
#'  data(ChickWeight)
#'  m <- civis_ml_sparse_linear_regressor(ChickWeight, dependent_variable = "weight")
#'  yhat <- fetch_oos_scores(m)
#'
#' # make a prediction job, storing in a redshift table
#' pred_info <- predict(m, newdata = civis_table("schema.table", "my_database"),
#'    output_table = "schema.scores_table")
#'
#' }
#' @export
civis_ml_sparse_linear_regressor <- function(x,
                                             dependent_variable,
                                             primary_key = NULL,
                                             excluded_columns = NULL,
                                             fit_intercept = TRUE,
                                             normalize = FALSE,
                                             fit_params = NULL,
                                             cross_validation_parameters = NULL,
                                             calibration = c(NULL, "sigmoid", "isotonic"),
                                             oos_scores_table = NULL,
                                             oos_scores_db = NULL,
                                             oos_scores_if_exists = c("fail", "append", "drop", "truncate"),
                                             model_name = NULL,
                                             cpu_requested = NULL,
                                             memory_requested = NULL,
                                             disk_requested = NULL,
                                             notifications = NULL,
                                             polling_interval = NULL,
                                             verbose = FALSE) {
  model_type <- "sparse_linear_regressor"
  params <- list(
    fit_intercept = fit_intercept,
    normalize = normalize
  )

  if (!fit_intercept & normalize) {
    warning("fit_intercept = FALSE, ignoring normalize.")
  }

  civis_ml(x = x, dependent_variable = dependent_variable,
           model_type = model_type, primary_key = primary_key,
           excluded_columns = excluded_columns, parameters = params,
           fit_params = fit_params,
           cross_validation_parameters = cross_validation_parameters,
           calibration = calibration, oos_scores_table = oos_scores_table,
           oos_scores_db = oos_scores_db,
           oos_scores_if_exists = oos_scores_if_exists, model_name = model_name,
           cpu_requested = cpu_requested, memory_requested = memory_requested,
           disk_requested = disk_requested, notifications = notifications,
           polling_interval = polling_interval, verbose = verbose)
}

#' CivisML Sparse Ridge Regression
#'
#' @inheritParams civis_ml
#' @inheritParams civis_ml_sparse_linear_regressor
#' @param alpha The regularization strength, must be a vector of floats of
#'   lenght n_targets or a single float. Larger values specify stronger
#'   regularization.
#' @param max_iter Maximum number of iterations for conjugate gradient solver.
#'   For \code{sparse_cg} and \code{lsqr} solvers, the default value is
#'   predetermined. For the \code{sag} solver, the default value is 1000.
#' @param tol Precision of the solution.
#' @param solver Solver to use for the optimization problem.
#'   \describe{
#'     \item{auto}{chooses the solver automatically based on the type of data.}
#'     \item{svd}{uses Singular Value Decomposition of X to compute the Ridge
#'       coefficients. More stable for singular matrices than \code{cholesky}.}
#'     \item{cholesky}{uses the standard decomposition to obtain a closed-form
#'       solution.}
#'     \item{sparse_cg}{uses the conjugate gradient solver. As an iterative
#'       algorithm, this solver is more appropriate than \code{cholesky} for
#'       large-scale data.}
#'     \item{lsqr}{uses the dedicated regularized least-squares routine.}
#'     \item{sag}{uses Stochastic Average Gradient descent. It also uses an
#'       iterative procedure, and is often faster than other solvers when both
#'       n_samples and n_features are large. Note that \code{sag} fast
#'       convergence is only guaranteed on features with approximately the same
#'       scale}
#'   }
#' @param random_state The seed of the pseudo random number generator to use
#'   when shuffling the data. Used only when \code{solver = "sag"}.
#' @examples
#' \dontrun{
#'  data(ChickWeight)
#'  m <- civis_ml_sparse_ridge_regressor(ChickWeight, dependent_variable = "weight", alpha = 999)
#'  yhat <- fetch_oos_scores(m)
#'
#'  # Grid search
#'  cv_params <- list(alpha = c(.001, .01, .1, 1))
#'  m <- civis_ml_sparse_ridge_regressor(ChickWeight,
#'    dependent_variable = "weight",
#'    cross_validation_parameters = cv_params,
#'    calibration = NULL)
#'
#' # make a prediction job, storing in a redshift table
#' pred_info <- predict(m, newdata = civis_table("schema.table", "my_database"),
#'    output_table = "schema.scores_table")
#' }
#' @inheritSection civis_ml Data Sources
#' @inherit civis_ml return
#' @export
civis_ml_sparse_ridge_regressor <- function(x,
                                            dependent_variable,
                                            primary_key = NULL,
                                            excluded_columns = NULL,
                                            alpha = 1.0,
                                            fit_intercept = TRUE,
                                            normalize = FALSE,
                                            max_iter = NULL,
                                            tol = 0.001,
                                            solver = c('auto', 'svd', 'cholesky', 'lsqr', 'sparse_cg', 'sag'),
                                            random_state = 42,
                                            fit_params = NULL,
                                            cross_validation_parameters = NULL,
                                            calibration = c(NULL, "sigmoid", "isotonic"),
                                            oos_scores_table = NULL,
                                            oos_scores_db = NULL,
                                            oos_scores_if_exists = c("fail", "append", "drop", "truncate"),
                                            model_name = NULL,
                                            cpu_requested = NULL,
                                            memory_requested = NULL,
                                            disk_requested = NULL,
                                            notifications = NULL,
                                            polling_interval = NULL,
                                            verbose = FALSE) {
  model_type <- "sparse_ridge_regressor"
  params <- list(
    alpha = alpha,
    fit_intercept = fit_intercept,
    normalize = normalize,
    max_iter = max_iter,
    tol = tol,
    solver = match.arg(solver),
    random_state = random_state
  )

  civis_ml(x = x, dependent_variable = dependent_variable,
           model_type = model_type, primary_key = primary_key,
           excluded_columns = excluded_columns, parameters = params,
           fit_params = fit_params,
           cross_validation_parameters = cross_validation_parameters,
           calibration = calibration, oos_scores_table = oos_scores_table,
           oos_scores_db = oos_scores_db,
           oos_scores_if_exists = oos_scores_if_exists, model_name = model_name,
           cpu_requested = cpu_requested, memory_requested = memory_requested,
           disk_requested = disk_requested, notifications = notifications,
           polling_interval = polling_interval, verbose = verbose)
}

#' CivisML Gradient Boosting Classifier
#'
#' @inheritParams civis_ml
#' @param loss The loss function to be optimized. \code{deviance} refers to
#'   deviance (logistic regression) for classification with probabilistic
#'   outputs. For \code{exponential}, gradient boosting recovers the AdaBoost
#'   algorithm.
#' @param learning_rate The learning rate shrinks the contribution of each tree
#'   by \code{learning_rate}. There is a trade-off between \code{learning_rate}
#'   and \code{n_estimators}.
#' @param n_estimators The number of boosting stages to perform. Gradient
#'   boosting is fairly robust to over-fitting, so a large number usually
#'   results in better predictive performance.
#' @param subsample The fraction of samples to be used for fitting individual
#'   base learners. If smaller than 1.0, this results in Stochastic Gradient
#'   Boosting. \code{subsample} interacts with the parameter \code{n_estimators}.
#'   Choosing \code{subsample < 1.0} leads to a reduction of variance and an
#'   increase in bias.
#' @param criterion The function to measure the quality of a split. The default
#'   value \code{criterion = "friedman_mse"} is generally the best as it can
#'   provide a better approximation in some cases.
#' @param min_samples_split The minimum number of samples required to split
#'   an internal node. If an integer, then consider \code{min_samples_split}
#'   as the minimum number. If a float, then \code{min_samples_split} is a
#'   percentage and \code{ceiling(min_samples_split * n_samples)} are the
#'   minimum number of samples for each split.
#' @param min_samples_leaf The minumum number of samples required to be in
#'   a leaf node. If an integer, then consider \code{min_samples_leaf} as the
#'   minimum number. If a float, the \code{min_samples_leaf} is a percentage
#'   and \code{ceiling(min_samples_leaf * n_samples)} are the minimum number
#'   of samples for each leaf node.
#' @param min_weight_fraction_leaf The minimum weighted fraction of the sum
#'   total of weights required to be at a leaf node.
#' @param max_depth Maximum depth of the individual regression estimators. The
#'   maximum depth limits the number of nodes in the tree. Tune this parameter
#'   for best performace. The best value depends on the interaction of the
#'   input variables.
#' @param min_impurity_split Threshold for early stopping in tree growth. A node
#'   will split if its impurity is above the threshold, otherwise it is a leaf.
#' @param random_state The seed of the random number generator.
#' @param max_features The number of features to consider when looking for the
#'   best split.
#'   \describe{
#'     \item{integer}{consider \code{max_features} at each split.}
#'     \item{float}{then \code{max_features} is a percentage and
#'       \code{max_features * n_features} are considered at each split.}
#'     \item{auto}{then \code{max_features = sqrt(n_features)}}
#'     \item{sqrt}{then \code{max_features = sqrt(n_features)}}
#'     \item{log2}{then \code{max_features = log2(n_features)}}
#'     \item{NULL}{then \code{max_features = n_features}}
#'   }
#' @param max_leaf_nodes Grow trees with \code{max_leaf_nodes} in best-first
#'   fashion. Best nodes are defined as relative reduction to impurity. If
#'   \code{max_leaf_nodes = NULL} then unlimited number of leaf nodes.
#' @param presort Whether to presort the data to speed up the finding of best
#'   splits in fitting.
#' @inheritSection civis_ml Data Sources
#' @inherit civis_ml return
#' @examples
#' \dontrun{
#'  df <- iris
#'  names(df) <- stringr::str_replace(names(df), "\\.", "_")
#'
#'  m <- civis_ml_gradient_boosting_classifier(df,
#'    dependent_variable = "Species",
#'    learning_rate = .01,
#'    n_estimators = 100,
#'    subsample = .5,
#'    max_depth = 5,
#'    max_features = NULL)
#'  yhat <- fetch_oos_scores(m)
#'
#' # Grid Search
#' cv_params <- list(
#'    n_estimators = c(100, 200, 500),
#'    learning_rate = c(.01, .1),
#'    max_depth = c(2, 3))
#'
#' m <- civis_ml_gradient_boosting_classifier(df,
#'    dependent_variable = "Species",
#'    subsample = .5,
#'    max_features = NULL,
#'    cross_validation_parameters = cv_params)
#'
#' pred_info <- predict(m,  civis_table("schema.table", "my_database"),
#'    output_table = "schema.scores_table")
#' }
#' @export
civis_ml_gradient_boosting_classifier <- function(x,
                                                  dependent_variable,
                                                  primary_key = NULL,
                                                  excluded_columns = NULL,
                                                  loss = c('deviance', 'exponential'),
                                                  learning_rate = 0.1,
                                                  n_estimators = 500,
                                                  subsample = 1.0,
                                                  criterion = c('friedman_mse', 'mse', 'mae'),
                                                  min_samples_split = 2,
                                                  min_samples_leaf = 1,
                                                  min_weight_fraction_leaf = 0.0,
                                                  max_depth = 2,
                                                  min_impurity_split = 1e-7,
                                                  random_state = 42,
                                                  max_features = 'sqrt',
                                                  max_leaf_nodes = NULL,
                                                  presort = c('auto', TRUE, FALSE),
                                                  fit_params = NULL,
                                                  cross_validation_parameters = NULL,
                                                  calibration = c(NULL, "sigmoid", "isotonic"),
                                                  oos_scores_table = NULL,
                                                  oos_scores_db = NULL,
                                                  oos_scores_if_exists = c("fail", "append", "drop", "truncate"),
                                                  model_name = NULL,
                                                  cpu_requested = NULL,
                                                  memory_requested = NULL,
                                                  disk_requested = NULL,
                                                  notifications = NULL,
                                                  polling_interval = NULL,
                                                  verbose = FALSE) {

  model_type <- "gradient_boosting_classifier"
  params <- list(
    loss = match.arg(loss),
    learning_rate = learning_rate,
    n_estimators = n_estimators,
    subsample = subsample,
    criterion = match.arg(criterion),
    min_samples_split = min_samples_split,
    min_samples_leaf = min_samples_leaf,
    min_weight_fraction_leaf = min_weight_fraction_leaf,
    max_depth = max_depth,
    min_impurity_split = min_impurity_split,
    random_state = random_state,
    max_leaf_nodes = max_leaf_nodes,
    presort = match.arg(presort)
  )

  if (is.character(max_features)) {
    stopifnot(max_features %in% c("sqrt", "auto", "log2"))
  }
  params$max_features <- max_features

  civis_ml(x = x, dependent_variable = dependent_variable,
           model_type = model_type, primary_key = primary_key,
           excluded_columns = excluded_columns, parameters = params,
           fit_params = fit_params,
           cross_validation_parameters = cross_validation_parameters,
           calibration = calibration, oos_scores_table = oos_scores_table,
           oos_scores_db = oos_scores_db,
           oos_scores_if_exists = oos_scores_if_exists, model_name = model_name,
           cpu_requested = cpu_requested, memory_requested = memory_requested,
           disk_requested = disk_requested, notifications = notifications,
           polling_interval = polling_interval, verbose = verbose)
}

#' CivisML Gradient Boosting Regressor
#'
#' @inheritParams civis_ml
#' @inheritParams civis_ml_gradient_boosting_classifier
#'
#' @param loss The loss function to be optimized. \code{ls} refers to least
#'   squares regression. \code{lad} (least absolute deviation) is a highly
#'   robust loss function solely based on order information of the input
#'   variables. \code{huber} is a combination of the two. \code{quantile}
#'   allows quantile regression (use \code{alpha} to specify the quantile).
#' @param alpha The alpha-quantile of the \code{huber} loss function and the
#'   \code{quantile} loss function. Ignored unless \code{loss = "huber"} or
#'   \code{loss = "quantile"}
#'
#' @inheritSection civis_ml Data Sources
#' @inherit civis_ml return
#' @examples
#' \dontrun{
#' data(ChickWeight)
#'
#' m <- civis_ml_gradient_boosting_regressor(ChickWeight,
#'   dependent_variable = "weight",
#'   learning_rate = .01,
#'   n_estimators = 100,
#'   subsample = .5,
#'   max_depth = 5,
#'   max_features = NULL)
#' yhat <- fetch_oos_scores(m)
#'
#' # Grid Search
#' cv_params <- list(
#'   n_estimators = c(100, 200, 500),
#'   learning_rate = c(.01, .1),
#'   max_depth = c(2, 3))
#'
#' m <- civis_ml_gradient_boosting_regressor(ChickWeight,
#'   dependent_variable = "weight",
#'   subsample = .5,
#'   max_features = NULL,
#'   cross_validation_parameters = cv_params)
#'
#' pred_info <- predict(m,  civis_table("schema.table", "my_database"),
#'    output_table = "schema.scores_table")
#' }
#' @export
civis_ml_gradient_boosting_regressor <- function(x,
                                                 dependent_variable,
                                                 primary_key = NULL,
                                                 excluded_columns = NULL,
                                                 loss = c('ls', 'lad', 'huber', 'quantile'),
                                                 learning_rate = 0.1,
                                                 n_estimators = 500,
                                                 subsample = 1.0,
                                                 criterion = c('friedman_mse', 'mse', 'mae'),
                                                 min_samples_split = 2,
                                                 min_samples_leaf = 1,
                                                 min_weight_fraction_leaf = 0.0,
                                                 max_depth = 2,
                                                 min_impurity_split = 1e-7,
                                                 random_state = 42,
                                                 max_features = 'sqrt',
                                                 alpha = 0.9,
                                                 max_leaf_nodes = NULL,
                                                 presort = c('auto', TRUE, FALSE),
                                                 fit_params = NULL,
                                                 cross_validation_parameters = NULL,
                                                 calibration = c(NULL, "sigmoid", "isotonic"),
                                                 oos_scores_table = NULL,
                                                 oos_scores_db = NULL,
                                                 oos_scores_if_exists = c("fail", "append", "drop", "truncate"),
                                                 model_name = NULL,
                                                 cpu_requested = NULL,
                                                 memory_requested = NULL,
                                                 disk_requested = NULL,
                                                 notifications = NULL,
                                                 polling_interval = NULL,
                                                 verbose = FALSE) {

  model_type <- "gradient_boosting_regressor"
  params <- list(
    loss = match.arg(loss),
    learning_rate = learning_rate,
    n_estimators = n_estimators,
    subsample = subsample,
    criterion = match.arg(criterion),
    min_samples_split = min_samples_split,
    min_samples_leaf = min_samples_leaf,
    min_weight_fraction_leaf = min_weight_fraction_leaf,
    max_depth = max_depth,
    min_impurity_split = min_impurity_split,
    random_state = random_state,
    alpha = alpha,
    max_leaf_nodes = max_leaf_nodes,
    presort = match.arg(presort)
  )

  if (is.character(max_features)) {
    stopifnot(max_features %in% c("sqrt", "auto", "log2"))
  }
  params$max_features <- max_features

  civis_ml(x = x, dependent_variable = dependent_variable,
           model_type = model_type, primary_key = primary_key,
           excluded_columns = excluded_columns, parameters = params,
           fit_params = fit_params,
           cross_validation_parameters = cross_validation_parameters,
           calibration = calibration, oos_scores_table = oos_scores_table,
           oos_scores_db = oos_scores_db,
           oos_scores_if_exists = oos_scores_if_exists, model_name = model_name,
           cpu_requested = cpu_requested, memory_requested = memory_requested,
           disk_requested = disk_requested, notifications = notifications,
           polling_interval = polling_interval, verbose = verbose)
}

#' CivisML Random Forest Classifier
#'
#' @inheritParams civis_ml
#' @inheritParams civis_ml_gradient_boosting_classifier
#' @inheritParams civis_ml_sparse_logistic
#'
#' @param criterion The function to measure the quality of a split. Supported
#'   criteria are \code{gini} for the Gini impurity and \code{entropy} for the
#'   information gain.
#' @param bootstrap Whether bootstrap samples are used when building trees.
#'
#' @inheritSection civis_ml Data Sources
#' @inherit civis_ml return
#' @examples
#' \dontrun{
#'  df <- iris
#'  names(df) <- stringr::str_replace(names(df), "\\.", "_")
#'
#'  m <- civis_ml_random_forest_classifier(df,
#'    dependent_variable = "Species",
#'    n_estimators = 100,
#'    max_depth = 5,
#'    max_features = NULL)
#'  yhat <- fetch_oos_scores(m)
#'
#' # Grid Search
#' cv_params <- list(
#'    n_estimators = c(100, 200, 500),
#'    max_depth = c(2, 3))
#'
#'  m <- civis_ml_random_forest_classifier(df,
#'    dependent_variable = "Species",
#'    max_features = NULL,
#'    cross_validation_parameters = cv_params)
#'
#' pred_info <- predict(m,  civis_table("schema.table", "my_database"),
#'    output_table = "schema.scores_table")
#' }
#' @export
civis_ml_random_forest_classifier <- function(x,
                                              dependent_variable,
                                              primary_key = NULL,
                                              excluded_columns = NULL,
                                              n_estimators = 500,
                                              criterion = c('gini', 'entropy'),
                                              max_depth = NULL,
                                              min_samples_split = 2,
                                              min_samples_leaf = 1,
                                              min_weight_fraction_leaf = 0.0,
                                              max_features = 'sqrt',
                                              max_leaf_nodes = NULL,
                                              min_impurity_split = 1e-7,
                                              bootstrap = TRUE,
                                              random_state = 42,
                                              class_weight = NULL,
                                              fit_params = NULL,
                                              cross_validation_parameters = NULL,
                                              calibration = c(NULL, "sigmoid", "isotonic"),
                                              oos_scores_table = NULL,
                                              oos_scores_db = NULL,
                                              oos_scores_if_exists = c("fail", "append", "drop", "truncate"),
                                              model_name = NULL,
                                              cpu_requested = NULL,
                                              memory_requested = NULL,
                                              disk_requested = NULL,
                                              notifications = NULL,
                                              polling_interval = NULL,
                                              verbose = FALSE) {

  model_type <- "random_forest_classifier"
  params <- list(
    n_estimators = n_estimators,
    criterion = match.arg(criterion),
    max_depth = max_depth,
    min_samples_split = min_samples_split,
    min_samples_leaf = min_samples_leaf,
    min_weight_fraction_leaf = min_weight_fraction_leaf,
    max_leaf_nodes = max_leaf_nodes,
    min_impurity_split = min_impurity_split,
    bootstrap = bootstrap,
    random_state = random_state
  )

  if (is.character(max_features)) {
    stopifnot(max_features %in% c("sqrt", "auto", "log2"))
  }
  params$max_features <- max_features

  if (!is.null(class_weight)) {
    if (is.character(class_weight)) {
      stopifnot(class_weight == "balanced")
    } else {
      stopifnot(is.list(class_weight))
    }
    params$class_weight <- class_weight
  }

  civis_ml(x = x, dependent_variable = dependent_variable,
           model_type = model_type, primary_key = primary_key,
           excluded_columns = excluded_columns, parameters = params,
           fit_params = fit_params,
           cross_validation_parameters = cross_validation_parameters,
           calibration = calibration, oos_scores_table = oos_scores_table,
           oos_scores_db = oos_scores_db,
           oos_scores_if_exists = oos_scores_if_exists, model_name = model_name,
           cpu_requested = cpu_requested, memory_requested = memory_requested,
           disk_requested = disk_requested, notifications = notifications,
           polling_interval = polling_interval, verbose = verbose)
}

#' CivisML Random Forest Regressor
#'
#' @inheritParams civis_ml_random_forest_classifier
#'
#' @param criterion The function used to measure the quality of a split.
#'   Supported criteria are \code{mse} for the mean squared error, and \code{mae}
#'   for the mean absolute error.
#'
#' @inheritSection civis_ml Data Sources
#' @inherit civis_ml return
#' @examples
#' \dontrun{
#' data(ChickWeight)
#'
#' m <- civis_ml_random_forest_regressor(ChickWeight,
#'   dependent_variable = "weight",
#'   n_estimators = 100,
#'   max_depth = 5,
#'   max_features = NULL)
#' yhat <- fetch_oos_scores(m)
#'
#' # Grid Search
#' cv_params <- list(
#'   n_estimators = c(100, 200, 500),
#'   max_depth = c(2, 3))
#'
#' m <- civis_ml_random_forest_regressor(ChickWeight,
#'   dependent_variable = "weight",
#'   max_features = NULL,
#'   cross_validation_parameters = cv_params)
#'
#' pred_info <- predict(m,  civis_table("schema.table", "my_database"),
#'    output_table = "schema.scores_table")
#' }
#' @export
civis_ml_random_forest_regressor <- function(x,
                                             dependent_variable,
                                             primary_key = NULL,
                                             excluded_columns = NULL,
                                             n_estimators = 500,
                                             criterion = c('mse', 'mae'),
                                             max_depth = NULL,
                                             min_samples_split = 2,
                                             min_samples_leaf = 1,
                                             min_weight_fraction_leaf = 0.0,
                                             max_features = 'sqrt',
                                             max_leaf_nodes = NULL,
                                             min_impurity_split = 1e-7,
                                             bootstrap = TRUE,
                                             random_state = 42,
                                             fit_params = NULL,
                                             cross_validation_parameters = NULL,
                                             calibration = c(NULL, "sigmoid", "isotonic"),
                                             oos_scores_table = NULL,
                                             oos_scores_db = NULL,
                                             oos_scores_if_exists = c("fail", "append", "drop", "truncate"),
                                             model_name = NULL,
                                             cpu_requested = NULL,
                                             memory_requested = NULL,
                                             disk_requested = NULL,
                                             notifications = NULL,
                                             polling_interval = NULL,
                                             verbose = FALSE) {

  model_type <- "random_forest_regressor"
  params <- list(
    n_estimators = n_estimators,
    criterion = match.arg(criterion),
    max_depth = max_depth,
    min_samples_split = min_samples_split,
    min_samples_leaf = min_samples_leaf,
    min_weight_fraction_leaf = min_weight_fraction_leaf,
    max_leaf_nodes = max_leaf_nodes,
    min_impurity_split = min_impurity_split,
    bootstrap = bootstrap,
    random_state = random_state
  )

  if (is.character(max_features)) {
    stopifnot(max_features %in% c("sqrt", "auto", "log2"))
  }
  params$max_features <- max_features

  civis_ml(x = x, dependent_variable = dependent_variable,
           model_type = model_type, primary_key = primary_key,
           excluded_columns = excluded_columns, parameters = params,
           fit_params = fit_params,
           cross_validation_parameters = cross_validation_parameters,
           calibration = calibration, oos_scores_table = oos_scores_table,
           oos_scores_db = oos_scores_db,
           oos_scores_if_exists = oos_scores_if_exists, model_name = model_name,
           cpu_requested = cpu_requested, memory_requested = memory_requested,
           disk_requested = disk_requested, notifications = notifications,
           polling_interval = polling_interval, verbose = verbose)
}

#' CivisML Extra Trees Classifier
#'
#' @inheritParams civis_ml_random_forest_classifier
#'
#' @inheritSection civis_ml Data Sources
#' @inherit civis_ml return
#' @examples
#' \dontrun{
#'  df <- iris
#'  names(df) <- stringr::str_replace(names(df), "\\.", "_")
#'
#'  m <- civis_ml_extra_trees_classifier(df,
#'    dependent_variable = "Species",
#'    n_estimators = 100,
#'    max_depth = 5,
#'    max_features = NULL)
#'  yhat <- fetch_oos_scores(m)
#'
#' # Grid Search
#' cv_params <- list(
#'    n_estimators = c(100, 200, 500),
#'    max_depth = c(2, 3))
#'
#'  m <- civis_ml_extra_trees_classifier(df,
#'    dependent_variable = "Species",
#'    max_features = NULL,
#'    cross_validation_parameters = cv_params)
#'
#' pred_info <- predict(m,  civis_table("schema.table", "my_database"),
#'    output_table = "schema.scores_table")
#' }
#' @export
civis_ml_extra_trees_classifier <- function(x,
                                            dependent_variable,
                                            primary_key = NULL,
                                            excluded_columns = NULL,
                                            n_estimators = 500,
                                            criterion = c('gini', 'entropy'),
                                            max_depth = NULL,
                                            min_samples_split = 2,
                                            min_samples_leaf = 1,
                                            min_weight_fraction_leaf = 0.0,
                                            max_features = 'sqrt',
                                            max_leaf_nodes = NULL,
                                            min_impurity_split = 1e-7,
                                            bootstrap = FALSE,
                                            random_state = 42,
                                            class_weight = NULL,
                                            fit_params = NULL,
                                            cross_validation_parameters = NULL,
                                            calibration = c(NULL, "sigmoid", "isotonic"),
                                            oos_scores_table = NULL,
                                            oos_scores_db = NULL,
                                            oos_scores_if_exists = c("fail", "append", "drop", "truncate"),
                                            model_name = NULL,
                                            cpu_requested = NULL,
                                            memory_requested = NULL,
                                            disk_requested = NULL,
                                            notifications = NULL,
                                            polling_interval = NULL,
                                            verbose = FALSE) {

  model_type <- "extra_trees_classifier"
  params <- list(
    n_estimators = n_estimators,
    criterion = match.arg(criterion),
    max_depth = max_depth,
    min_samples_split = min_samples_split,
    min_samples_leaf = min_samples_leaf,
    min_weight_fraction_leaf = min_weight_fraction_leaf,
    max_leaf_nodes = max_leaf_nodes,
    min_impurity_split = min_impurity_split,
    bootstrap = bootstrap,
    random_state = random_state
  )

  if (is.character(max_features)) {
    stopifnot(max_features %in% c("sqrt", "auto", "log2"))
  }
  params$max_features <- max_features

  if (!is.null(class_weight)) {
    if (is.character(class_weight)) {
      stopifnot(class_weight == "balanced")
    } else {
      stopifnot(is.list(class_weight))
    }
    params$class_weight <- class_weight
  }

  civis_ml(x = x, dependent_variable = dependent_variable,
           model_type = model_type, primary_key = primary_key,
           excluded_columns = excluded_columns, parameters = params,
           fit_params = fit_params,
           cross_validation_parameters = cross_validation_parameters,
           calibration = calibration, oos_scores_table = oos_scores_table,
           oos_scores_db = oos_scores_db,
           oos_scores_if_exists = oos_scores_if_exists, model_name = model_name,
           cpu_requested = cpu_requested, memory_requested = memory_requested,
           disk_requested = disk_requested, notifications = notifications,
           polling_interval = polling_interval, verbose = verbose)
}

#' CivisML Extra Trees Regressor
#'
#' @inheritParams civis_ml_random_forest_regressor
#'
#' @inheritSection civis_ml Data Sources
#' @inherit civis_ml return
#' @examples
#' \dontrun{
#' data(ChickWeight)
#'
#' m <- civis_ml_extra_trees_regressor(ChickWeight,
#'   dependent_variable = "weight",
#'   n_estimators = 100,
#'   max_depth = 5,
#'   max_features = NULL)
#' yhat <- fetch_oos_scores(m)
#'
#' # Grid Search
#' cv_params <- list(
#'   n_estimators = c(100, 200, 500),
#'   max_depth = c(2, 3))
#'
#' m <- civis_ml_extra_trees_regressor(ChickWeight,
#'   dependent_variable = "weight",
#'   max_features = NULL,
#'   cross_validation_parameters = cv_params)
#'
#' pred_info <- predict(m,  civis_table("schema.table", "my_database"),
#'    output_table = "schema.scores_table")
#' }
#' @export
civis_ml_extra_trees_regressor <- function(x,
                                           dependent_variable,
                                           primary_key = NULL,
                                           excluded_columns = NULL,
                                           n_estimators = 500,
                                           criterion = c('mse', 'mae'),
                                           max_depth = NULL,
                                           min_samples_split = 2,
                                           min_samples_leaf = 1,
                                           min_weight_fraction_leaf = 0.0,
                                           max_features = 'sqrt',
                                           max_leaf_nodes = NULL,
                                           min_impurity_split = 1e-7,
                                           bootstrap = FALSE,
                                           random_state = 42,
                                           fit_params = NULL,
                                           cross_validation_parameters = NULL,
                                           calibration = c(NULL, "sigmoid", "isotonic"),
                                           oos_scores_table = NULL,
                                           oos_scores_db = NULL,
                                           oos_scores_if_exists = c("fail", "append", "drop", "truncate"),
                                           model_name = NULL,
                                           cpu_requested = NULL,
                                           memory_requested = NULL,
                                           disk_requested = NULL,
                                           notifications = NULL,
                                           polling_interval = NULL,
                                           verbose = FALSE) {

  model_type <- "extra_trees_regressor"
  params <- list(
    n_estimators = n_estimators,
    criterion = match.arg(criterion),
    max_depth = max_depth,
    min_samples_split = min_samples_split,
    min_samples_leaf = min_samples_leaf,
    min_weight_fraction_leaf = min_weight_fraction_leaf,
    max_leaf_nodes = max_leaf_nodes,
    min_impurity_split = min_impurity_split,
    bootstrap = bootstrap,
    random_state = random_state
  )

  if (is.character(max_features)) {
    stopifnot(max_features %in% c("sqrt", "auto", "log2"))
  }
  params$max_features <- max_features

  civis_ml(x = x, dependent_variable = dependent_variable,
           model_type = model_type, primary_key = primary_key,
           excluded_columns = excluded_columns, parameters = params,
           fit_params = fit_params,
           cross_validation_parameters = cross_validation_parameters,
           calibration = calibration, oos_scores_table = oos_scores_table,
           oos_scores_db = oos_scores_db,
           oos_scores_if_exists = oos_scores_if_exists, model_name = model_name,
           cpu_requested = cpu_requested, memory_requested = memory_requested,
           disk_requested = disk_requested, notifications = notifications,
           polling_interval = polling_interval, verbose = verbose)
}
