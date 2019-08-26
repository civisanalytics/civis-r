#!/usr/bin/env Rscript
library(civis)


args <- commandArgs(trailingOnly = TRUE)
task_file_id <- as.numeric(args[[1]])

fut <- read_civis(task_file_id, using = readRDS)

# install and load missing packages
cat("Installing and loading required packages", fill = TRUE)

pkgs <- fut$packages
if (length(pkgs) > 0) {
  pkg_list <- pkgs[!(pkgs %in% rownames(installed.packages()))]
  if (length(pkg_list) > 0) install.packages(pkg_list, repos = "https://cloud.r-project.org")
  req_results <- lapply(fut$packages, require, character.only = TRUE, quietly = TRUE)
}

# run it. Leaving it without tryCatch so that errors are surfaced immediately in platform.

cat("Evaluating R expression", fill = TRUE)

attach(fut$envir)

res <- eval(fut$expr)

detach(fut$envir)

cat("Complete.", fill = TRUE)


# store results on s3
output_file_id <- write_civis_file(res)
cat("Output file id: ", output_file_id, fill = TRUE)

# attach to the job output
script_id <- Sys.getenv("CIVIS_JOB_ID")
run_id <- Sys.getenv("CIVIS_RUN_ID")
resp <- scripts_post_containers_runs_outputs(script_id, run_id,
                                              object_type = "File",
                                              object_id = output_file_id)
