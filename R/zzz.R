.onLoad <- function(libname, pkgname) {
  options(
    civis.ml_train_template_id = get_train_template_id(),
    civis.default_database = NULL
  )
  invisible()
}

get_train_template_id <- function() {

  civis_ml_template_ids <- get_template_ids_all_versions()

  id <- civis_ml_template_ids[civis_ml_template_ids$version == "prod" &
                              civis_ml_template_ids$name == "training", "id"]

  return(id)

}


