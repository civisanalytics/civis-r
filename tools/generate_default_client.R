# To generate a new client, save the default civis api spec locally
# and pass the filename as an arg to this script.  i.e.
# Rscript generate_default_client.R

library(civis)

job <- scripts_post_custom(from_template_id = 9925)
run <- scripts_post_custom_runs(job$id)
await(scripts_get_custom_runs, id = job$id, run_id = run$id)
spec_id <- scripts_list_custom_runs_outputs(job$id, run$id)[[1]]$objectId
fn <- tempfile(fileext = ".json")
download_civis(spec_id, file = fn)

Sys.setenv("R_CLIENT_DEV" = "")
Sys.setenv("CIVIS_API_KEY" = "")

api_spec <- jsonlite::fromJSON(fn, simplifyVector = FALSE)

# To pass R CMD CHECK, this has to be an 'R Data file' (rda) produced by save.
save(api_spec, file = "../R/sysdata.rda")
client_str <- civis:::generate_client(api_spec)
civis:::write_client(client_str, FILENAME = paste0("../", civis:::FILENAME))
devtools::document()
