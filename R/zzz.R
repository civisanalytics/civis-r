.onLoad <- function(libname, pkgname) {
  options(
    civis.ml_train_template_id_prev = 9112,
    civis.ml_predict_template_id_prev = 9113,
    civis.ml_train_template_id = 9968,
    civis.ml_predict_template_id = 9969,
    civis.default_database = NULL
  )
  invisible()
}
