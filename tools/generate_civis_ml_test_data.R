library(civis)
library(future)

start <- proc.time()
cat("setting up models...", fill = TRUE)
plan("multisession", workers = 20)

class_algo <- c("sparse_logistic", "gradient_boosting_classifier",
                "random_forest_classifier", "extra_trees_classifier")
reg_algo <- paste0(c("sparse_linear", "sparse_ridge", "gradient_boosting",
                     "random_forest", "extra_trees"), "_regressor")

data("iris")
tmp <- tempfile()
write.csv(iris, file = tmp)
iris_id <- write_civis_file(tmp, name = "iris.csv")

do_class <- function(algo, id) {
  future(civis_ml(civis_file(id), dependent_variable = "Species", model_type = algo))
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
  future(civis_ml(civis_file(id), "weight", algo))
}
unlink(tmp)

cat("submitting models...", fill = T)

mclass_fut <- lapply(class_algo, do_class, id = iris_id)
mreg_fut <- lapply(reg_algo, do_regr, id = chick_id)
multi_output_fut <- future(civis_ml(civis_file(chick_id), dependent_variable = c("weight", "Time"),
                                model_type = "random_forest_regressor"))
multi_class_output_fut <- future(civis_ml(civis_file(chick_id), dependent_variable = c("Time", "Diet"),
                                          model_type = "random_forest_classifier"))

cat("waiting for models to complete...", fill = TRUE)

mclass_list <- lapply(c(mclass_fut, binary_fut), value)
cat("classification models completed", fill = TRUE)

mreg_list <- lapply(mreg_fut, value)
cat("regression models completed", fill = TRUE)

multi_output <- list(value(multi_output_fut), value(multi_class_output_fut))
cat("multi output completed", fill = TRUE)

ms <- c(mclass_list, mreg_list, multi_output)

cat("writing tests/testthat/data/civis_ml_models.rds", fill = TRUE)
saveRDS(ms, "../tests/testthat/data/civis_ml_models.rds")

end <- proc.time()
tot <- end - start

cat("total time: ", tot[3], "s", fill = TRUE)
