.onLoad <- function(libname, pkgname) {
  options(
    civis.ml_train_template_id = get_train_template_id(),
    civis.default_database = NULL
  )
  invisible()
}

get_train_template_id <- function() {

  CIVIS_ML_TEMPLATE_IDS <- get_template_ids_all_versions()

  id <- CIVIS_ML_TEMPLATE_IDS[CIVIS_ML_TEMPLATE_IDS$version == "prod" &
                              CIVIS_ML_TEMPLATE_IDS$name == "training", "id"]

  return(id)

}


