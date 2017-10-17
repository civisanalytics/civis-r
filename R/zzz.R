.onLoad <- function(libname, pkgname) {
  op <- options()
  op.civis <- list(
    civis.ml_train_template_id = 9968,
    civis.ml_predict_template_id = 9969,
    civis.default_database = NULL
  )

  toset <- !(names(op.civis) %in% names(op))
  if (any(toset)) options(op.civis[toset])

  invisible()
}
