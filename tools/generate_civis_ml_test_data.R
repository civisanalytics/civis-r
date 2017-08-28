library(civis)
library(future)

cat("setting up models...", fill = TRUE)
plan("multisession", workers = 20)

class_algo <- c("sparse_logistic", "gradient_boosting_classifier",
                "random_forest_classifier", "extra_trees_classifier")
reg_algo <- paste0(c("sparse_linear", "sparse_ridge", "gradient_boosting",
                     "random_forest", "extra_trees"), "_regressor")

do_class <- function(algo) {
  future(civis_ml(civis_table("datascience.iris", "redshift-general"),
           dependent_variable = "type",
           model_type = algo))
}

data("ChickWeight")
tmp <- tempfile()
write.csv(ChickWeight, file = tmp)
id <- write_civis_file(tmp, name = "chicweight.csv")

do_regr <- function(algo, id) {
  future(civis_ml(civis_file(id), "weight", algo))
}
unlink(tmp)


cat("submitting models...", fill = T)

mclass_fut <- lapply(class_algo, do_class)
mreg_fut <- lapply(reg_algo, do_regr, id = id)
multi_output_fut <- future(civis_ml(civis_file(id), dependent_variable = c("weight", "Time"),
                                model_type = "random_forest_regressor"))

cat("waiting for models to complete...", fill = TRUE)
mclass_list <- lapply(mclass_fut, value)
cat("classification models completed", fill = TRUE)
mreg_list <- lapply(mreg_fut, value)
cat("regression models completed", fill = TRUE)
multi_output <- value(multi_output_fut)
cat("multi output completed", fill = TRUE)
ms <- c(mclass_list, mreg_list, list(multi_output))

cat("writing tests/testthat/data/civis_ml_models.rds", fill = T)
saveRDS(ms, "../tests/testthat/data/civis_ml_models.rds")
