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

  # only check update if API key is set
  if (!inherits(try(api_key(), silent = TRUE), "try-error")) {
    if (length(scripts_list_custom(id)) == 0) {
      # use the previous version; assume that only the latest version is in internal release
      prev <- versions[length(versions) - 1]
      id <- CIVIS_ML_TEMPLATE_IDS[CIVIS_ML_TEMPLATE_IDS$version == prev &
                                  CIVIS_ML_TEMPLATE_IDS$name == "train", "id"]
    }
  }
  return(id)
}
