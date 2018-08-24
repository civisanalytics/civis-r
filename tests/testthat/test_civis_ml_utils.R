library(civis)
context("civis_ml_utils")

model_list <- readRDS("data/civis_ml_models.rds")
is_classif <- sapply(model_list, function(m) is(m, "civis_ml_classifier"))
is_noval <- sapply(model_list, function(m) is.null(m$metrics))

job_ids <- lapply(model_list, function(x) purrr::pluck(x, "job", "id"))
run_ids <- lapply(model_list, function(x) purrr::pluck(x, "run", "id"))
id_regex <- purrr:::map2(paste0("(", job_ids, ")*"), paste0("(", run_ids, ")"), paste)
class_algo <- sapply(model_list[is_classif], function(x) x$model_info$model$model)
reg_algo <- sapply(model_list[!is_classif], function(x) x$model_info$model$model)

coef_mods <- model_list[c(1, 7, 8, 9, 18)]
no_coef_mods <- model_list[!(model_list %in% coef_mods)]

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
  n_models <- length(reg_msg)

  first_row <- lapply(reg_msg, purrr::pluck, 1)
  expect_true(all(stringr::str_detect(first_row, reg_algo)))

  third_row <- lapply(reg_msg, purrr::pluck, 3)
  expect_true(all(str_detect_multiple(third_row, id_regex[!is_classif])))

  expect_true(all(stringr::str_detect(reg_msg[1:(n_models - 1)], "(MAD)*(RMSE)*(R-squared)")))
  expect_true(all(stringr::str_detect(reg_msg[1:(n_models - 1)], c("weight"))))
  expect_true(all(stringr::str_detect(reg_msg[n_models - 1], c("(weight)*(Time)"))))
})

test_that("print.civis_ml digits works", {
  m <- model_list[!is_classif][[1]]
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
})

test_that("get_metrics throws error if not model", {
  msg <- "is_civis_ml\\(model\\) is not TRUE"
  expect_error(get_metric("HIPPO_INA_STRING"), msg)
})

test_that("get_model_data returns model data", {
  for (m in model_list) {
    dat <- m$model_info$data
    expect_equal(dat, get_model_data(m))
    expect_equal(get_model_data(m, "target_columns"),
                 m$model_info$data$target_columns)
  }
})

test_that("is_multiclass works", {
  expect_true(is_multiclass(model_list[[1]]))
  # binary
  expect_false(is_multiclass(model_list[[7]]))
  # reg
  expect_false(is_multiclass(model_list[!is_classif][[1]]))
})

test_that("is_multitarget works", {
  test <- sapply(model_list, is_multitarget)
  ans <- sapply(model_list, function(m) length(m$model_info$data$n_unique_targets) > 1)
  expect_equal(test, ans)
})

test_that("get_predict_template_id returns correct template for train/predict version ", {
  m <- model_list[[1]]
  id <- m$job$fromTemplateId
  ver <- CIVIS_ML_TEMPLATE_IDS[CIVIS_ML_TEMPLATE_IDS$id == id, "version"]
  pred_id <- CIVIS_ML_TEMPLATE_IDS[CIVIS_ML_TEMPLATE_IDS$version == ver &
                                   CIVIS_ML_TEMPLATE_IDS$name == "predict", "id"]
  expect_equal(get_predict_template_id(m), pred_id)

  fake_model <- list(job = list(fromTemplateId = 9112))
  expect_equal(get_predict_template_id(fake_model), 9113)
})

test_that("get_train_template_id reverts to last id if others not available", {
  id <- with_mock(
    `civis:::api_key` = function(...) "key",
    `civis::scripts_list_custom` = function(...) list(),
    get_train_template_id())
  ans <- CIVIS_ML_TEMPLATE_IDS[1, "id"]
  expect_equal(id, ans)
})

test_that("coef.civis_ml returns correct coefficients when available", {
    true_coefs <- readRDS("data/model_coefficients.rds")
    test_coefs <- lapply(coef_mods, coef)
    expect_equal(true_coefs, test_coefs)
})

test_that("coef.civis_ml returns NULL when coefficients are unavailable", {
  for (m in no_coef_mods) {
    expect_null(coef(m))
  }
})
