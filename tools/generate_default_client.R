# To generate a new client, save the default civis api spec locally
# and pass the filename as an arg to this script.  i.e.
# Rscript generate_default_client.R default_api_spec.json

source("../R/client_base.R")
source("../R/generate_client.R")
source("../R/utils.R")

args <- commandArgs(trailingOnly = TRUE)
api_spec <- jsonlite::fromJSON(args[1], simplifyVector=FALSE)

# To pass R CMD CHECK, this has to be an 'R Data file' (rda) produced by save.
save(api_spec, file = "../R/sysdata.rda")
client_str <- generate_client(api_spec)
write_client(client_str, FILENAME = paste0("../", FILENAME))
devtools::document()
