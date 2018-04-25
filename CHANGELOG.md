# Changelog

## Unreleased

### Changed
- Add v2.2 CivisML templates.


## [1.3.0] - 2018-03-30

### Added

- Local dataframes can now be sent to CivisML as feather files.
- Print CivisML training warnings when model results are fetched from Platform.
- `predict.civis_ml` and `create_and_run_pred` gain a `dvs_to_predict` argument which allows users to restrict output predictions to a subset of targets from a multi-output model.

- `query_civis_file` exports a `"schema.tablename"`, `sql("query")`, or existing sql script id to S3 and returns the file id.
- `write_civis` gains a `diststyle` argument for controlling the distribution of tables on Redshift.

### Changed

- CivisML stacking documentation has been updated.
- `write_civis_file` now defaults to the file path for the `name` argument instead of requiring it.

### Fixed

- `read_civis.numeric`, `write_civis.numeric`, `download_civis.numeric`, and `query_civis.numeric` now fail with a better error message when called with `x = NA` of type numeric.
- The `hidden` argument of `write_civis` now works.
- New lines are removed from parameter descriptions in generated docs.

## [1.2.0] - 2018-01-23

### Fixed

- `resolved.CivisFuture` now gets the status of the platform job directly and updates the state of the future.

### Added

- Update `civis_ml` documentation with the following CivisML 2.1
features and defaults:
	- Nulls are permitted in single-column dependent variables, and
    will be dropped before modeling.
	- Hyperband is enabled for stacking models.
	- MLPs can be run without hyperband (and R checks removed)
	- `n_jobs` is dynamically calculated by default for training as
    well as prediction.
	- `feather-format` is now allowed as an input format.
- CivisML vignette updated with new v2 features.
- `value.CivisFuture` now prints error logs from failed jobs, throws a catcheable
error, and fetches the job logs automatically.

### Changed

- `civis_ml_fetch_existing` can handle runs with missing
  `metrics.json` files.
- `civis_ml` will use the most recent CivisML version available, falling back to CivisML 1.1.

## [1.1.1] - 2017-11-20

### Fixed

- `write_civis` calls `imports_post_files` correctly after a change in the argument order.

## [1.1.0] - 2017-11-10

### Fixed

- Corrected project_id type in `publish_rmd` and `publish_html`
- `polling_interval` can now be specified in `civis_ml`

### Changed

- `call_api` only retries on 413, 429, 502, 503, 504 as in civis-python.
- Updated the default api spec to the current spec. New endpoints added
include /announcements, /apps, /clusters, /codes, /enhancements,
/media, /notebooks.  Most endpoints were already available to all users,
but with this change these endpoints will now be documented.
- `write_civis_file` uploads objects larger than 50MB
in multiple parts, and supports objects up to 5TB. 
Uploads occur in parallel if a `future::plan` has been specified.
- Files uploaded using `write_civis_file` now don't expire by default. 

### Added

- "Publish to Civis" Rstudio addin installed by default that publishes `.Rmd` and `.html` files as Civis Platform reports.
- `write_civis.numeric` is provided to sync a CSV file on S3 to Redshift given a file id.
- A new `plan(civis_platform)` has been added to evaluate R expressions on Civis Platform 
using the `future` API. 

#### CivisML
- New named workflows: `multilayer_perceptron_regressor`, `multilayer_perceptron_classifier`,
`stacking_regressor`,  and `stacking_classifier`
- Hyperband is provided for hyperparameter tuning by setting `cross_validation_parameters = "hyperband"` in `gradient_boosting_classifier`, `random_forest_classifier`, `extra_trees_classifier`, `multilayer_perceptron_classifier`, `gradient_boosting_regressor`, `random_forest_regressor`, `extra_trees_regressor`, and `multilayer_perceptron_regressor`.
- Hyperparameter tuning (grid search and hyperband) is now distributed across EC2 instances by setting `n_jobs > 1` (default 4)
- The validation step can now be skipped by setting `validation_data = "skip"`.
- Compute resources for prediction jobs can now also be set using `"cpu_requested"`, `"memory_requested"`, and `"disk_requested"`
arguments in `predict.civis_ml`.

## [1.0.2] - 2017-09-21

### Fixed

- roxygen2::roxygenize, devtools::build, and memoise::memoise included in NAMESPACE
- uuid dependency removed
- run_generate_client now in R/generate_client.R
- skip autogen on windows with R < 3.4.0
- don't test time formatting in fetch logs.

## [1.0.1] - 2017-09-18

### Fixed

- `configure` and `configure.win` now use `R_HOME` to find `Rscript` instead of assuming that `Rscript` is on the path.
- `configure` uses `bin/sh` instead of `bin/bash`
- `configure.win` uses `Rscript.exe` for windows builds
- `configure.win` does not use `bin/bash`.

## [1.0.0] - 2017-09-08

### Added 

#### CivisML

- Print methods for `civis_ml` objects have been improved.
- `fetch_predictions` is provided to read results from a prediction job into memory from S3.
- Plot method for `civis_ml_regressor` shows y-yhat binned histogram using ggplot2.
- Plot method for `civis_ml_classifier` shows the decile plot using ggplot2.
- Hist methods for `civis_ml` objects provide histograms of the OOS scores using ggplot2.
- A list of estimator names is provided in `CIVIS_ML_CLASSIFIERS` and `CIVIS_ML_REGRESSORS`.

#### Programming

- `fetch_logs` now works for any `civis_api` object with a valid `*_runs_logs` method, and any `civis_error` object.

#### Documentation

- A [quick start](https://civisanalytics.github.io/civis-r/articles/quick_start.html) vignette was added.
- A [CivisML](https://civisanalytics.github.io/civis-r/articles/civis_ml.html) modeling vignette was added.
- Only whitelisted endpoints are documented on the web.
- The `expires_at` parameter of `write_civis_file` has been clarified.


### Changed

- `civis_ml` now throws an appropriate error message if a model type is called with multiple dependent variables but doesn't support them.
- Errors from CivisML are now errors in `civis_ml`, with the CivisML error logs being printed to the console.
- A `empty_result_error` is now thrown from `read_civis.sql` if the query returns no results.
- `as_function` deprecation from `purrr` replaced.
- If there is only one available database and a default database has not been set as a package option or provided, `get_default_database` uses that database.

### Fixed

- `read_civis` now reads from a temporary file when the file size is > than 2^31-1 bytes.

## [0.9.1] - 2017-08-15

### Added


#### CivisML

- `civis_ml` wrapper for CivisML 1.1.
- Named civis_ml workflows: `civis_ml_sparse_logistic`, `civis_ml_gradient_boosting_classifier`, `civis_ml_random_forest_classifier`, `civis_ml_extra_trees_classifier`, `civis_ml_sparse_linear_regressor`, `civis_ml_sparse_ridge_regressor`,
`civis_ml_gradient_boosting_regressor`, `civis_ml_random_forest_regressor`, and
`civis_ml_extra_trees_regressor`.
- `civis_table`, `civis_file` used to indicate data sources for `civis_ml`
- `predict.civis_ml` runs a CivisML scoring job on platform for a given model and data.
- `fetch_logs` can be used to fetch logs from a model run.
- `fetch_oos_scores` can be used to return out of sample/fold scores from a model.
- `civis_ml_fetch_existing` returns an existing platform job.

#### IO

- `read_civis` reads a`"schema.table"`, a `sql("query")`, or a file id to a data frame.
- `read_civis` sql queries are read only.
- `write_civis`  uploads a data frame or local csv to Redshift.
- `write_civis_file` uploads a serialized R object or an unserialized local file to the files endpoint (S3).
- `query_civis` runs an arbitrary `"query"`, `sql("query")`, or a query id.
- `query_civis` only returns query meta data.
- `download_civis` downloads a`"schema.table"`, a `sql("query")`, or a file id to a file.
- `download_civis` can export in parallel using `split = TRUE`.
- A default database for all IO functions can be stored in `options(civis.default_db = "my_database")`.

#### Tables
- `refresh_table` can be used to refresh table meta data
- `transfer_table` can be used to transfer tables between redshift databases
- `get_table_id` can be used to retrieve a table id

#### dplyr

-  DBI interface: `dbWriteTable`, `dbSentStatement`, `dbReadTable`, `dbListTables`
- dplyr and dbplyr are suggested.

#### Reports
- `publish_rmd`, `publish_html` return the report id.
- Rmarkdown is the default option for `publish_rmd`.

#### Programming
- API responses are S3 objects.
- `print.civis_api` method for API responses hides attributes.
- `await` and `await_all` are provided for polling job/task completion.
- `await` and `await_all` use an exponential backoff retry time for polling if `.interval` is not set (the default).
- `await` and `await_all` throw a `civis_await_error` on platform errors, and print the platform error message along with job and run ids.
- `get_error` can be used to obtain additional debugging data from `civis_await_error` (platform errors).
- `await` and `await_all` throw a `civis_timeout_error` if `.timeout` was specified.
- `NULL` can be passed directly to the API.

#### Packaging
- New endpoints can be generated by reinstalling the package.
- Naming scheme for generated endpoints matches the Python client, e.g. `scripts_post_sql`
- `LICENCE` is now BSD-3.
- `CODE_OF_CONDUCT.md`
- `CHANGELOG.md`

#### Documentation

- IO vignette.
- Concurrent polling using futures and foreach vignette.
- Package website.



