library(civis)
context("civis_ml_utils")

model_list <- readRDS("data/civis_ml_models.rds")
is_classif <- sapply(model_list, function(m) is(m, "civis_ml_classifier"))
is_noval <- sapply(model_list, function(m) is.null(m$metrics))

job_ids <- lapply(model_list, function(x) x[["job"]][["id"]])
run_ids <- lapply(model_list, function(x) x[["run"]][["id"]])
id_regex <- paste(paste0("(", job_ids, ")*"), paste0("(", run_ids, ")"))
class_algo <- sapply(model_list[is_classif], function(x) x$model_info$model$model)
reg_algo <- sapply(model_list[!is_classif], function(x) x$model_info$model$model)

feat_imp_mods <- model_list[c(2, 3, 4, 10, 11, 12, 15, 16)]
feat_imp_err_mods <- model_list[!(model_list %in% feat_imp_mods)]
coef_mods <- model_list[c(1, 7, 8, 9, 18)]
no_coef_mods <- model_list[!(model_list %in% coef_mods)]


str_detect_multiple <- function(string, pattern) {
  mapply(function(string, pattern) grepl(pattern, string),
         string = string, pattern = pattern)
}

test_that("get_template_ids_all_versions", {

  fake_template_alias_objects <- list(list(id = 11,
                                           objectId = 12367,
                                           objectType = "template_script",
                                           alias = "civis-ShapefileExport",
                                           userId = 3001,
                                           displayName = "Export Shapefile"),
                                      list(id = 14,
                                           objectId = 11219,
                                           objectType = "template_script",
                                           alias = "civis-civisml-training",
                                           userId = 400,
                                           displayName = "Model Training"),
                                      list(id = 21,
                                           objectId = 10615,
                                           objectType = "template_script",
                                           alias = "civis-civisml-training-dev",
                                           userId = 400,
                                           displayName = "Model Training - DEV ONLY"),
                                      list(id = 26,
                                           objectId = 11221,
                                           objectType = "template_script",
                                           alias = "civis-civisml-registration-v2-2",
                                           userId = 1750,
                                           displayName = "Trained Model Registration, v2.2"))

  with_mock(
     `civis::fetch_until` = function(...) fake_template_alias_objects,

      expect_equal(get_template_ids_all_versions(),
                   data.frame(id=c(11219,10615,11221),
                              version=c("prod","dev","v2.2"),
                              name=c("training","training","registration"),
                              stringsAsFactors=FALSE)
                   )
      )

})


test_that("get_job_type_version works", {

  expect_equal(get_job_type_version("civis-civisml-training"),
               list(job_type = "training", version = "prod"))
  expect_equal(get_job_type_version("civis-civisml-training-v2-3"),
               list(job_type = "training", version = "v2.3"))
  expect_equal(get_job_type_version("civis-civisml-training-dev"),
               list(job_type = "training", version = "dev"))
  expect_equal(get_job_type_version("civis-civisml-training-foo-bar"),
               list(job_type = "training", version = "foo-bar"))

  expect_error(get_job_type_version("foo-bar"))
  expect_error(get_job_type_version("civis-civisml"))
  expect_error(get_job_type_version("civis-civisml-"))
  expect_error(get_job_type_version("civis-civisml-training-"))
  expect_error(get_job_type_version("civis-civisml-training-foobar-"))

})


test_that("print.civis_ml_classifier works", {
  class_msg <- lapply(model_list[is_classif], function(x) utils::capture.output(x))

  first_row <- lapply(class_msg, function(x) x[[1]])
  mapply(expect_match, first_row, class_algo)

  third_row <- lapply(class_msg, function(x) x[[3]])
  expect_true(all(str_detect_multiple(third_row, id_regex[is_classif])))
})

test_that("print.civis_ml_regressor works", {
  reg_msg <- lapply(model_list[!is_classif], function(x) utils::capture.output(x))
  n_models <- length(reg_msg)

  first_row <- lapply(reg_msg, function(x) x[[1]])
  mapply(expect_match, first_row, reg_algo)

  third_row <- lapply(reg_msg, function(x) x[[3]])
  expect_true(all(str_detect_multiple(third_row, id_regex[!is_classif])))

  reg_msg <- lapply(reg_msg, paste0, collapse = '')
  lapply(reg_msg[1:(n_models - 1)], expect_match, "(MAD)*(RMSE)*(R-squared)")
  lapply(reg_msg[1:(n_models - 1)], expect_match, "weight")
  lapply(reg_msg[n_models - 1], expect_match, c("(weight)*(Time)"))
})

test_that("print.civis_ml digits works", {
  m <- model_list[!is_classif][[1]]
  d_str <- capture.output(print(m, digits = 2))[6:8]
  nums <- lapply(strsplit(d_str, " "), tail, 1)
  dec <- lapply(sapply(nums, strsplit, "\\."), tail, 1)
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
  civis_ml_template_ids <- get_template_ids_all_versions()
  ver <- civis_ml_template_ids[civis_ml_template_ids$id == id, "version"]
  pred_id <- civis_ml_template_ids[civis_ml_template_ids$version == ver[1] &
                                   civis_ml_template_ids$name == "prediction", "id"]
  expect_equal(get_predict_template_id(m), pred_id)

  fake_model <- list(job = list(fromTemplateId = 9112))
  expect_equal(get_predict_template_id(fake_model), 9113)
})


test_that("get_feature_importance returns correct feature importance matrix when available", {
  true_feature_importances <- readRDS("data/feature_importances.rds")
  test_feature_importances <- lapply(feat_imp_mods, get_feature_importance)
  expect_equal(true_feature_importances, test_feature_importances)
})

test_that("models with no feature importance throw errors for get_feature_importance", {
  for (m in feat_imp_err_mods) {
    expect_error(get_feature_importance(m), "Feature importance data not available.")
  }
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
