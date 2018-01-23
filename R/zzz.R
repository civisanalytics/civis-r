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

  if (!inherits(try(api_key(), silent = TRUE), "try-error")) {
    i <- 1
    while (length(scripts_list_custom(id)) == 0 &&
           i < length(versions)) {
      # use the previous version; assume that only the latest version is in internal release
      prev <- versions[length(versions) - i]
      id <- CIVIS_ML_TEMPLATE_IDS[CIVIS_ML_TEMPLATE_IDS$version == prev &
                                    CIVIS_ML_TEMPLATE_IDS$name == "train", "id"]
      i <- i + 1
    }
  }
  return(id)
}


