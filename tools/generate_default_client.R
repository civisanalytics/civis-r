# To generate a new client, run as:
# Rscript generate_default_client.R

library(civis)
source("../R/generate_client.R")

cat("Running robot template script...", fill = TRUE)
robot_script_id <- 9925
job <- scripts_post_custom(robot_script_id)
run <- scripts_post_custom_runs(job$id)
res <- await(scripts_get_custom_runs, id = job$id, run_id = run$id)
fileid <- scripts_list_custom_runs_outputs(id = job$id, run_id = run$id)[[1]]$objectId
cat("Downloading robot user spec...", fill = TRUE)
fn <- tempfile(fileext = ".json")
download_civis(fileid, fn)
api_spec <- jsonlite::fromJSON(fn, simplifyVector = FALSE)

# To pass R CMD CHECK, this has to be an 'R Data file' (rda) produced by save.
cat("saving sysdata.rda...", fill = TRUE)
save(api_spec, file = "../R/sysdata.rda")
cat("generating default client...", fill = TRUE)
client_str <- civis:::generate_client(api_spec)
civis:::write_client(client_str, FILENAME = paste0("../", FILENAME))
devtools::document()
