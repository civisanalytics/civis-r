## ---- eval=FALSE--------------------------------------------------------------
#  nap <- function(seconds) {
#      Sys.sleep(seconds)
#  }
#  
#  start <- Sys.time()
#  nap(1)
#  nap(2)
#  nap(3)
#  end <- Sys.time()
#  print(end - start)

## ---- eval=FALSE--------------------------------------------------------------
#  library(future)
#  library(civis)
#  
#  # Define a concurrent backend with enough processes so each function
#  # we want to run concurrently has its own process. Here we'll need at least 2.
#  plan("multiprocess", workers=10)
#  
#  # Load data
#  data(iris)
#  data(airquality)
#  airquality <- airquality[!is.na(airquality$Ozone),]  # remove missing in dv
#  
#  # Create a future for each model, using the special %<-% assignment operator.
#  # These futures are created immediately, kicking off the models.
#  air_model %<-% civis_ml(airquality, "Ozone", "gradient_boosting_regressor")
#  iris_model %<-% civis_ml(iris, "Species", "sparse_logistic")
#  
#  # At this point, `air_model` has not finished training yet. That's okay,
#  # the program will just wait until `air_model` is done before printing it.
#  print("airquality R^2:")
#  print(air_model$metrics$metrics$r_squared)
#  print("iris ROC:")
#  print(iris_model$metrics$metrics$roc_auc)

## ---- eval=FALSE--------------------------------------------------------------
#  library(parallel)
#  library(doParallel)
#  library(foreach)
#  library(civis)
#  
#  # Register a local cluster with enough processes so each function
#  # we want to run concurrently has its own process. Here we'll
#  # need at least 3, with 1 for each model_type in model_types.
#  cluster <- makeCluster(10)
#  registerDoParallel(cluster)
#  
#  # Model types to build
#  model_types <- c("sparse_logistic",
#                   "gradient_boosting_classifier",
#                   "random_forest_classifier")
#  
#  # Load data
#  data(iris)
#  
#  # Listen for multiple models to complete concurrently
#  model_results <- foreach(model_type=iter(model_types), .packages='civis') %dopar% {
#      civis_ml(iris, "Species", model_type)
#  }
#  stopCluster(cluster)
#  print("ROC Results")
#  lapply(model_results, function(result) result$metrics$metrics$roc_auc)

## ---- eval=FALSE--------------------------------------------------------------
#  library(civis)
#  library(parallel)
#  
#  # Model types to build
#  model_types <- c("sparse_logistic",
#                   "gradient_boosting_classifier",
#                   "random_forest_classifier")
#  
#  # Load data
#  data(iris)
#  
#  # Loop over all models in parallel with a max of 10 processes
#  model_results <- mclapply(model_types, function(model_type) {
#    civis_ml(iris, "Species", model_type)
#  }, mc.cores=10)
#  
#  # Wait for all models simultaneously
#  print("ROC Results")
#  lapply(model_results, function(result) result$metrics$metrics$roc_auc)

