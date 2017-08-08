# The default client contains only the docs/functions in BASE_RESOURCES_V1

source("../R/client_base.R")
source("../R/generate_client.R")
source("../R/utils.R")

api_spec <- get_spec()
base_regex <- paste0("^/", BASE_RESOURCES_V1)

path_idx <- unlist(lapply(base_regex, function(regex, nam){ 
    grep(regex, nam)
  }, nam=names(api_spec$paths)))

api_spec$paths <- api_spec$paths[unique(path_idx)]

# To pass R CMD CHECK, this has to be an 'R Data file' (rda) produced by save.
save(api_spec, file = "../R/sysdata.rda")
client_str <- generate_client(api_spec, IGNORE=IGNORE)
write_client(client_str, FILENAME=paste0("../", FILENAME))
devtools::document()
