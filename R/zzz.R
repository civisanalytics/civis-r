.onLoad <- function(libname, pkgname) {
  options(
    civis.ml_train_template_id = get_train_template_id(),
    civis.default_database = NULL
  )
  invisible()
}

get_train_template_id <- function() {
  versions <- unique(CIVIS_ML_TEMPLATE_IDS$version)
  latest <- max(versions)
  id <- CIVIS_ML_TEMPLATE_IDS[CIVIS_ML_TEMPLATE_IDS$version == latest &
                              CIVIS_ML_TEMPLATE_IDS$name == "train", "id"]
  return(id)
}
