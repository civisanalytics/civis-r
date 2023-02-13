## ---- eval=FALSE--------------------------------------------------------------
#  name <- civis::users_list_me()$name
#  paste(name, "is really awesome!")

## ---- eval=FALSE--------------------------------------------------------------
#  library(civis)
#  
#  # First we'll load a dataframe of the famous iris dataset
#  data(iris)
#  
#  # We'll set a default database and define the table where want to
#  # store our data
#  options(civis.default_db = "my_database")
#  iris_tablename <- "my_schema.my_table"
#  
#  # Next we'll push the data to the database table
#  write_civis(iris, iris_tablename)
#  
#  # Great, now let's read it back
#  df <- read_civis(iris_tablename)
#  
#  # Hmmm, I'm more partial to setosa myself. Let's write a custom sql query.
#  # We'll need to wrap our query string in `sql` to let read_civis know we
#  # are passing in a sql command rather than a tablename.
#  query_str <- paste("SELECT * FROM", iris_tablename, "WHERE Species = 'setosa'")
#  iris_setosa <- read_civis(sql(query_str))
#  
#  # Now let's store this data along with a note as a serialized R object
#  # on a remote file system. We could store any object remotely this way.
#  data <- list(data = iris_setosa, special_note = "The best iris species")
#  file_id <- write_civis_file(data)
#  
#  # Finally, let's read back our data from the remote file system.
#  data2 <- read_civis(file_id)
#  data2[["special_note"]]
#  
#  ## [1] "The best iris species"

## ---- eval=FALSE--------------------------------------------------------------
#  library(civis)
#  
#  # It really is a great dataset
#  data(iris)
#  
#  # Gradient boosting or random forest, who will win?
#  gb_model <- civis_ml_gradient_boosting_classifier(iris, "Species")
#  rf_model <- civis_ml_random_forest_classifier(iris, "Species")
#  macroavgs <- list(gb_model = gb_model$metrics$metrics$roc_auc_macroavg,
#                    rf_model = rf_model$metrics$metrics$roc_auc_macroavg)
#  macroavgs
#  
#  ## $gb_model
#  ## [1] 0.9945333
#  ##
#  ## $rf_model
#  ## [1] 0.9954667

