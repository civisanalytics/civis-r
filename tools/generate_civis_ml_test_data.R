library(civis)
library(future)

start <- proc.time()
cat("setting up models...", fill = TRUE)
plan("multisession", workers = 20)

data("iris")
tmp <- tempfile()
write.csv(iris, file = tmp)
iris_id <- write_civis_file(tmp, name = "iris.csv")

do_class <- function(algo, id) {
  cv <- if (grepl("perceptron", algo)) "hyperband" else NULL
  future(civis_ml(civis_file(id), dependent_variable = "Species",
                  model_type = algo, cross_validation_parameters = cv))
}

# two class
binary_iris <- iris[with(iris, Species %in% c('virginica', 'setosa')), ]
binary_fut <- future(civis_ml(binary_iris, dependent_variable = "Species",
                       model_type = "sparse_logistic"))

data("ChickWeight")
tmp <- tempfile()
write.csv(ChickWeight, file = tmp)
chick_id <- write_civis_file(tmp, name = "chicweight.csv")

do_regr <- function(algo, id) {
  cv <- if (grepl("perceptron", algo)) "hyperband" else NULL
  future(civis_ml(civis_file(id), "weight", algo,
                  cross_validation_parameters = cv))
}
unlink(tmp)

cat("submitting models...", fill = T)

mclass_fut <- lapply(CIVIS_ML_CLASSIFIERS, do_class, id = iris_id)
mreg_fut <- lapply(CIVIS_ML_REGRESSORS, do_regr, id = chick_id)
multi_output_fut <- future(civis_ml(civis_file(chick_id), dependent_variable = c("weight", "Time"),
                                model_type = "random_forest_regressor"))
multi_class_output_fut <- future(civis_ml(civis_file(chick_id), dependent_variable = c("Time", "Diet"),
                                          model_type = "random_forest_classifier"))

no_val_reg_fut <- future(civis_ml(civis_file(chick_id), dependent_variable = c("weight", "Time"),
                   model_type = "random_forest_regressor", validation_data = "skip"))

no_val_class_fut <- future(civis_ml(civis_file(iris_id), dependent_variable = c("Species"),
                              model_type = "sparse_logistic", validation_data = "skip"))

cat("waiting for models to complete...", fill = TRUE)

mclass_list <- lapply(c(mclass_fut, binary_fut), value)
cat(" ...classification models completed", fill = TRUE)

mreg_list <- lapply(mreg_fut, value)
cat(" ...regression models completed", fill = TRUE)

multi_output <- list(value(multi_output_fut), value(multi_class_output_fut))
cat(" ...multi output completed", fill = TRUE)

no_val <- list(value(no_val_reg_fut), value(no_val_class_fut))
ms <- c(mclass_list, mreg_list, multi_output, no_val)
cat(" ...no validation completed", fill = TRUE)

cat("writing tests/testthat/data/civis_ml_models.rds", fill = TRUE)
saveRDS(ms, "../tests/testthat/data/civis_ml_models.rds")

end <- proc.time()
tot <- end - start

cat("total time: ", tot[3], "s", fill = TRUE)
