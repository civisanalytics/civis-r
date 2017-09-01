library(civis)
context("civis_ml_utils")

model_list <- readRDS("data/civis_ml_models.rds")
is_classif <- sapply(model_list, function(m) is(m, "civis_ml_classifier"))

job_ids <- lapply(model_list, function(x) purrr::pluck(x, "job", "id"))
run_ids <- lapply(model_list, function(x) purrr::pluck(x, "run", "id"))
id_regex <- purrr:::map2(paste0("(", job_ids, ")*"), paste0("(", run_ids, ")"), paste)
class_algo <- c("sparse_logistic", "gradient_boosting_classifier",
                "random_forest_classifier", "extra_trees_classifier",
                "sparse_logistic", "random_forest_classifier")
reg_algo <- paste0(c("sparse_linear", "sparse_ridge", "gradient_boosting",
                     "random_forest", "extra_trees", "random_forest"), "_regressor")

str_detect_multiple <- function(string, pattern) {
  mapply(function(string, pattern) stringr::str_detect(string, pattern),
         string = string, pattern = pattern)
}


test_that("print.civis_ml_classifier works", {
  class_msg <- lapply(model_list[is_classif], function(x) utils::capture.output(x))

  first_row <- lapply(class_msg, purrr::pluck, 1)
  expect_true(all(stringr::str_detect(first_row, class_algo)))

  third_row <- lapply(class_msg, purrr::pluck, 3)
  expect_true(all(str_detect_multiple(third_row, id_regex[is_classif])))
})

test_that("print.civis_ml_regressor works", {
  reg_msg <- lapply(model_list[!is_classif], function(x) utils::capture.output(x))

  first_row <- lapply(reg_msg, purrr::pluck, 1)
  expect_true(all(stringr::str_detect(first_row, reg_algo)))

  third_row <- lapply(reg_msg, purrr::pluck, 3)
  expect_true(all(str_detect_multiple(third_row, id_regex[!is_classif])))
  expect_true(all(stringr::str_detect(reg_msg, "(MAD)*(RMSE)*(R-squared)")))
  expect_true(all(stringr::str_detect(reg_msg[1:5], c("weight"))))
  expect_true(all(stringr::str_detect(reg_msg[6], c("(weight)*(Time)"))))
})

test_that("print.civis_ml digits works", {
  m <- model_list[[6]]
  d_str <- capture.output(print(m, digits = 2))[6:8]
  nums <- lapply(stringr::str_split(d_str, " "), tail, 1)
  dec <- lapply(purrr::flatten(lapply(nums, stringr::str_split, "\\.")), tail, 1)
  expect_equal(sapply(dec, nchar), rep(2, 3))
})

test_that("get_metrics returns metrics", {
  for (m in model_list) {
    metr <- m$metrics$metrics
    expect_equal(metr, get_metric(m))
  }
  expect_equal(get_metric(m, "mad"), m$metrics$metrics$mad)
})

test_that("get_metrics throws error if not model", {
  msg <- "is_civis_ml\\(model\\) is not TRUE"
  expect_error(get_metric("HIPPO_INA_STRING"), msg)
})

test_that("get_model_data returns model data", {
  for (m in model_list) {
    dat <- m$metrics$data
    expect_equal(dat, get_model_data(m))
  }
  expect_equal(get_model_data(m, "target_columns"),
               m$metrics$data$target_columns)
})

test_that("is_multiclass works", {
  expect_true(is_multiclass(model_list[[1]]))
  expect_false(is_multiclass(model_list[[5]]))
  expect_false(is_multiclass(model_list[[6]]))
})

test_that("is_multitarget works", {
  expect_true(is_multitarget(model_list[[11]]))
  expect_true(is_multitarget(model_list[[12]]))
  expect_false(is_multitarget(model_list[[2]]))
  expect_true(is_multitarget(model_list[[12]]) &
                all(is_multiclass(model_list[[12]])))
})
