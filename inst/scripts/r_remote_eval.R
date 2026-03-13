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

eval_env <- list2env(as.list(fut$globals), parent = parent.frame())

res <- eval(fut$expr, envir = eval_env)

cat("Complete.", fill = TRUE)

saveRDS(res, file = 'r-object.rds')
write_job_output("r-object.rds")
