## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  # create a container script with a parameter
#  script <- scripts_post_containers(
#    required_resources = list(cpu = 1024, memory = 50, diskSpace = 15),
#    docker_command = 'cd /package_dir && Rscript inst/run_script.R',
#    docker_image_name = 'civisanalytics/datascience-r',
#    name = 'SCRIPT NAME',
#    params = list(
#      list(name = 'NAME_OF_ENV_VAR',
#           label = 'Name User Sees',
#           type = 'string',
#           required = TRUE)
#    )
#  )
#  
#  # publish the container script as a template
#  template <- templates_post_scripts(script$id, name = 'TEMPLATE NAME', note = 'Markdown Docs')
#  
#  # run a template script, returning file ids of run outputs
#  out <- run_template(template$id)
#  
#  # post a file or JSONValue run output within a script
#  write_job_output('filename.csv')
#  json_values_post(jsonlite::toJSON(my_list), 'my_list.json')
#  
#  # get run output file ids of a script
#  out <- fetch_output_file_ids(civis_script(id))
#  
#  # get csv run outputs of a script
#  df <- read_civis(civis_script(id), regex = '.csv', using = read.csv)
#  
#  # get JSONValue run outputs
#  my_list <- read_civis(civis_script(id))
#  

## ---- eval = FALSE------------------------------------------------------------
#  source <- c('
#   print("Hello World!")
#  ')
#  job <- scripts_post_r(name = 'Hello!', source = source)

## ---- eval = FALSE------------------------------------------------------------
#  run <- scripts_post_r_runs(job$id)
#  
#  # check the status
#  scripts_get_r_runs(job$id, run$id)
#  
#  # automatically poll until the job completes
#  await(scripts_get_r_runs, id = job$id, run_id = run$id)

## -----------------------------------------------------------------------------
#  run_script <- function(source, name = 'Cool') {
#    job <- scripts_post_r(name = name, source = source)
#    run <- scripts_post_r_runs(job$id)
#    await(scripts_get_r_runs, id = job$id, run_id = run$id)
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  source <- c("
#   library(civis)
#   data(iris)
#   write.csv(iris, 'iris.csv')
#   job_id <- as.numeric(Sys.getenv('CIVIS_JOB_ID'))
#   run_id <- as.numeric(Sys.getenv('CIVIS_RUN_ID'))
#   file_id <- write_civis_file('iris.csv')
#   scripts_post_r_runs_outputs(job_id, run_id, object_type = 'File', object_id = file_id)
#  ")
#  run <- run_script(source)

## ---- eval=FALSE--------------------------------------------------------------
#  source <- c("
#   library(civis)
#   data(iris)
#   write.csv(iris, 'iris.csv')
#   write_job_output('iris.csv')
#  ")
#  run <- run_script(source)

## ---- eval=FALSE--------------------------------------------------------------
#  source <- c("
#   library(civis)
#   library(jsonlite)
#   my_farm <- list(cows = 1, ducks = list(mallard = 2, goldeneye = 1))
#   json_values_post(jsonlite::toJSON(my_farm), name = 'my_farm.json')
#  ")
#  run_farm <- run_script(source)

## -----------------------------------------------------------------------------
#  out <- scripts_list_r_runs_outputs(run$rId, run$id)
#  iris <- read_civis(out$objectId, using = read.csv)

## ---- eval = FALSE------------------------------------------------------------
#  # get csv run outputs
#  iris <- read_civis(civis_script(run$rId), regex = '.csv', using = read.csv)
#  
#  # get JSONValues
#  my_farm <- read_civis(civis_script(run_farm$rId))

## ---- eval=FALSE--------------------------------------------------------------
#  # Add 'params' and 'arguments' to run_script
#  run_script <- function(source, args, name = 'Cool') {
#    params <- list(          # params is a list of individual parameters
#      list(
#        name = 'PET_NAME',   # name of the environment variable with the user value
#        label = 'Pet Name',  # name displaayed to the user
#        type = 'string',     # type
#        required = TRUE      # required?
#      )
#    )
#    job <- scripts_post_r(name = name,
#                          source = source,
#                          params = params,
#                          arguments = args)
#    run <- scripts_post_r_runs(job$id)
#    await(scripts_get_r_runs, id = job$id, run_id = run$id)
#  }
#  
#  # Access the PET_NAME variable
#  source <- c('
#    library(civis)
#    pet_name <- Sys.getenv("PET_NAME")
#    msg <- paste0("Hello", pet_name, "!")
#    print(msg)
#  ')
#  
#  # Let's run it! Here we pass the argument 'Fitzgerald' to the
#  # parameter 'PET_NAME' that we created.
#  run_script(source, name = 'Pet Greeting', args = list(PET_NAME = 'Fitzgerald'))
#  

## ---- eval=FALSE--------------------------------------------------------------
#  params <- list(
#    list(
#      name = 'PET_NAME',
#      label = 'Pet Name',
#      type = 'string',
#      required = TRUE
#    )
#  )
#  job <- scripts_post_r(name = 'Pet Greeter',
#                        source = source,
#                        params = params)

## ---- eval=FALSE--------------------------------------------------------------
#  note <- c("
#  # Pet Greeter
#  
#  Greets your pet, given its name!
#  
#  For your pet to receive the greeting, it must be a Civis Platform
#  user with the ability to read.
#  
#  Parameters:
#    * Pet Name: string, Name of pet.
#  
#  
#  Returns:
#    * Nothing
#  ")
#  template <- templates_post_scripts(script_id = job$id, note = note, name = 'Pet Greeter')

## ---- eval=FALSE--------------------------------------------------------------
#  job <- scripts_post_custom(id, arguments = arguments, ...)
#  run <- scripts_post_custom_runs(job$id)
#  await(scripts_get_custom_runs, id = job$id, run_id = run$id)

## ---- eval = FALSE------------------------------------------------------------
#  out <- run_template(template$id, arguments = list(PET_NAME = 'CHARLES'))

## ---- eval = FALSE------------------------------------------------------------
#  # We might need to find the project id first
#  search_list(type = 'project', 'My project Name')
#  out <- run_template(template$id, arguments = list(PET_NAME = 'CHARLES'),
#                      target_project_id = project_id)

## ----eval=FALSE---------------------------------------------------------------
#  templates_patch_scripts(template_id$id, note = new_note)

## ---- eval = FALSE------------------------------------------------------------
#  source <- c('
#    library(civis)
#    pet_name <- Sys.getenv("PET_NAME")
#    msg <- paste0("Hello ", pet_name, "! Would you care for a sandwich?")
#    print(msg)
#  ')
#  scripts_patch_r(id = job$id, name = 'Pet Greeter',
#                  source = source,
#                  params = params)

## -----------------------------------------------------------------------------
#  templates_patch_scripts(template$id, archived = TRUE)

