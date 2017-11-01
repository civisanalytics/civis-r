.onLoad <- function(libname, pkgname) {
  ids <- get_default_template_ids()
  options(
    civis.ml_train_template_id = ids[1],
    civis.ml_predict_template_id = ids[2],
    civis.default_database = NULL
  )
  invisible()
}

get_default_template_ids <- function() {
  versions <- unique(CIVIS_ML_TEMPLATE_IDS$version)
  latest <- max(versions)
  ids <- subset(CIVIS_ML_TEMPLATE_IDS, version == latest)$id

  # only check update if API key is valid
  if (!inherits(try(api_key(), silent = TRUE), "try-error")) {
    if (length(scripts_list_custom(ids[1])) == 0) {
      # use the previous version; assume that only the latest version is in internal release
      prev <- versions[length(versions) - 1]
      ids <- subset(CIVIS_ML_TEMPLATE_IDS, version == prev)$id
    }
  }
  return(ids)
}
