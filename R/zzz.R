.onLoad <- function(libname, pkgname) {
  ids <- get_template_ids()
  options(
    civis.ml_train_template_id = ids["civis_ml_train_id"],
    civis.ml_predict_template_id = ids["civis_ml_predict_id"],
    civis.default_database = NULL
  )
  invisible()
}

get_template_ids <- function() {
  # default: v 2.0
  civis_ml_train_id <- 9968
  civis_ml_predict_id <- 9969

  # only check update if API key is valid
  if (!inherits(try(api_key(), silent = TRUE), "try-error")) {
    if (length(scripts_list_custom(civis_ml_train_id)) == 0) {
      civis_ml_train_id <- 9112
      civis_ml_predict_id = 9113
    }
  }
  return(c(civis_ml_train_id = civis_ml_train_id,
           civis_ml_predict_id = civis_ml_predict_id))
}
