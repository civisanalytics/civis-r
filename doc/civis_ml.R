## ---- eval = FALSE------------------------------------------------------------
#  library(civis)
#  
#  civis_ml(df, ...)
#  civis_ml("path/to/data.csv", ...)
#  civis_ml(civis_table(table_name = "schema.table", database_name = "database"), ...)
#  civis_ml(civis_file(1234), ...)

## ---- eval  = FALSE-----------------------------------------------------------
#  options(civis.default_db = "my_database")
#  tab <- civis_table(table_name = "sample_project.premium_training_set")

## ---- eval = FALSE------------------------------------------------------------
#  library(civis)
#  tab <- civis_table("sample_project.premium_training_set")
#  m   <- civis_ml(tab, dependent_variable = "upgrade",
#                  model_type = "random_forest_classifier",
#                  primary_key = "brandable_user_id",
#                  excluded_columns = "residential_zip")
#  
#  m <- civis_ml_random_forest_classifier(tab,
#        primary_key = "brandable_user_id",
#        excluded_columns = "residential_zip")

## ---- eval = FALSE------------------------------------------------------------
#  tab <- civis_table("sample_project.premium_training_set")
#  
#  # hyperband
#  m_hyper <- civis_ml(tab, dependent_variable = "upgrade",
#                model_type = "random_forest_classifier",
#                primary_key = "brandable_user_id",
#                excluded_columns = "residential_zip",
#                cross_validation_parameters = 'hyperband')
#  
#  # grid search
#  cv_params <- list("max_depth" = c(2, 3, 5),
#                    "n_estimators" = c(50, 100, 500))
#  
#  m_grid <- civis_ml(tab, dependent_variable = "upgrade",
#                model_type = "random_forest_classifier",
#                primary_key = "brandable_user_id",
#                excluded_columns = "residential_zip",
#                cross_validation_parameters = cv_params)
#  
#  

## ---- eval = FALSE------------------------------------------------------------
#  m_stack <- civis_ml(tab, dependent_variable = "upgrade",
#                model_type = "stacking_classifier",
#                primary_key = "brandable_user_id",
#                excluded_columns = "residential_zip")

## ---- eval=FALSE--------------------------------------------------------------
#  m

## ----run_model, eval=FALSE, echo=FALSE----------------------------------------
#  # use this chunk to actually update the model if necessary
#  library(civis)
#  tab       <- civis_table("sample_project.premium_training_set")
#  cv_params <- list("max_depth" = c(2, 3, 5),
#                    "n_estimators" = c(50, 100, 500))
#  
#  
#  m <- civis_ml(tab, dependent_variable = "upgrade",
#                model_type = "random_forest_classifier",
#                primary_key = "brandable_user_id",
#                excluded_columns = "residential_zip",
#                cross_validation_parameters = cv_params)
#  saveRDS(m, file = "../inst/civis_ml_brandable.rds")
#  
#  oos <- fetch_oos_scores(m)
#  saveRDS(oos, file = "../inst/civis_ml_oos.rds")
#  
#  err_m <- tryCatch({
#    civis_ml(tab, dependent_variable = "upgrade",
#             model_type = "random_fest_classifier",
#             primary_key = "brandable_user_id",
#             excluded_columns = "residential_zip",
#             cross_validation_parameters = cv_params)
#    }, error = function(e) e)
#  saveRDS(err_m, file = "../inst/civis_ml_err.rds")
#  

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
library(civis)
path <- system.file("civis_ml_brandable.rds", package = 'civis')
m <- readRDS(path)
m

## -----------------------------------------------------------------------------
get_metric(m, "accuracy")
get_metric(m, "confusion_matrix")
get_metric(m, "roc_auc")

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  oos <- fetch_oos_scores(m)
#  head(oos)

## ---- echo=FALSE, eval=TRUE---------------------------------------------------
path <- system.file("civis_ml_oos.rds", package = 'civis')
oos <- readRDS(path)
head(oos)

## ---- fig.width = 5-----------------------------------------------------------
plot(m)

## -----------------------------------------------------------------------------
hist(m)

## ---- eval=FALSE--------------------------------------------------------------
#  pred_tab <- civis_table(table_name = "sample_project.brandable_all_users")
#  pred_job <- predict(m, newdata = pred_tab,
#                      output_table = "sample_project.brandable_user_scores")

## ---- eval=FALSE--------------------------------------------------------------
#  pred_job <- predict(m, newdata = pred_tab,
#                      output_table = "sample_project.brandable_user_scores",
#                      n_jobs = 25)

## ---- eval=FALSE--------------------------------------------------------------
#  yhat <- fetch_predictions(pred_job)

## ---- eval=FALSE--------------------------------------------------------------
#  # download from S3
#  download_civis(pred_job$model_info$output_file_ids, path = "my_predictions.csv")
#  
#  # download from Redshift
#  download_civis("sample_project.brandable_user_scores")

## ---- eval=FALSE--------------------------------------------------------------
#  model_id <- m$job$id
#  m <- civis_ml_fetch_existing(model_id)

## ---- eval=FALSE--------------------------------------------------------------
#  civis_ml(tab, dependent_variable = "upgrade",
#           model_type = "random_fest_classifier",
#           primary_key = "brandable_user_id",
#           excluded_columns = "residential_zip",
#           cross_validation_parameters = cv_params)

## ---- echo=FALSE, eval=TRUE---------------------------------------------------
path <- system.file("civis_ml_err.rds", package = 'civis')
err <- readRDS(path)
err

## ---- eval = FALSE------------------------------------------------------------
#  e <- tryCatch({
#    civis_ml(tab, dependent_variable = "upgrade",
#          model_type = "random_fest_classifier",
#          primary_key = "brandable_user_id",
#          excluded_columns = "residential_zip")
#    }, civis_ml_error = function(e) e)
#  get_error(e)
#  fetch_logs(e)

## ---- eval=FALSE--------------------------------------------------------------
#  retry_model <- function(max_retries = 5) {
#    i <- 1
#    while (i < max_retries) {
#      tryCatch({
#        m <- civis_ml(tab, dependent_variable = "upgrade",
#                 model_type = "random_forest_classifier",
#                 primary_key = "brandable_user_id",
#                 excluded_columns = "residential_zip")
#        return(m)
#      }, civis_ml_error = function(e) stop(e))
#      cat("Retry: ", i, fill = TRUE)
#      i <- i + 1
#    }
#    stop("Exceeded maximum retries.")
#  }

