source("R/generate_client.R")
source("R/client_base.R")
source("R/utils.R")

if (Sys.getenv("R_CLIENT_DEV") != "TRUE") {
  message("Generating API")
  spec <- get_spec()
  client_str <- generate_client(spec, IGNORE = IGNORE)

  message(paste0("Writing API to ", FILENAME))
  write_client(client_str, FILENAME = FILENAME)

  devtools::document()
} else {
  message("Skipping client generation")
}
