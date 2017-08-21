Civis Data Science API Client
================
[![Build Status](https://travis-ci.org/civisanalytics/civis-r.svg?branch=master)](https://travis-ci.org/civisanalytics/civis-r)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/civis)](https://cran.r-project.org/package=civis)

Introduction
------------

`civis` is an R package that helps analysts and developers interact with
the Civis Platform. The package includes a set of tools around common
workflows as well as a convenient interface to make requests directly to
the Civis Data Science API. See the [full documentation](https://civisanalytics.github.io/civis-r)
for more details.

Installation
------------

Installing and using `civis` requires an API key.  Instructions
for creating an API key can be found [here](https://civis.zendesk.com/hc/en-us/articles/216341583-Generating-an-API-Key).
All API keys have a set expiration date and so a new key will need to be
created at least every 30 days.

Once you have created an API key, you will then need to add it to your
environment so `civis` can access it. To do this, add the following
line to your `.Renviron` file:

```bash
CIVIS_API_KEY=adlfk942l2ka0dd0232
```

Be sure to replace the fake key `adlfk942l2ka0dd0232` with your newly
created key.

With an API key in place, you can now install `civis` using devtools:

```r
devtools::install_github("civisanalytics/civis-r", build_vignettes = TRUE)
```

:heavy_exclamation_mark: You must keep your API key secret. If you use version
control tools, ensure you do not commit `.Renviron` or any scripts in which
you have hard-coded an API key.

Usage
-----

`civis` includes functionality for both

1. Making direct calls to the API
2. Making single calls to accomplish a specific task (which may involve
making multiple calls to the API)

Functions which make direct calls to the API are prefixed with the name of the
resource that the function accesses.  For example, the `users` resource
encapsulates all the functionality of Platform regarding users.  We can make
various calls to the `users` resource to get information about ourselves and our
team.

```r
# Data about me
civis::users_list_me()

<civis_api>
List of 14
 $ id                      : int 971
 $ name                    : chr "A User"
 $ email                   : chr "a_user@example.com"
 $ username                : chr "a_user"
 $ initials                : chr "AU"
# ...
```

```r
# Data about my team
my_team <- civis::users_list()
team_members <- sapply(my_team, function(x) x$name)
print(team_members)

[1] "Jean-Luc Picard"      "Beverly Crusher"     "Q"
```

Many useful tasks will require making multiple direct calls to the API.
In order to make this easier, `civis` includes a number of wrapper functions
to make common tasks easier. For example, reading data from a table in
Platform is as easy as

```r
library(civis)

# Read an entire table in to memory
my_table <- "schema.tablename"
df <- read_civis(my_table, database="my_database")

# Run a query and read the results into memory
query <- sql("SELECT a, b, c FROM schema.tablename WHERE b > 42")
df2 <- read_civis(query, database="my_database")
```

`civis` includes many more functions for tasks like writing data to tables
and files as well as for creating reports. For more detailed documentation,
see the included vignettes:

```r
browseVignettes('civis')
```

Updating
--------
The `civis` package automatically generates R functions and documentation to interact with the Civis Platform when the package installs. Periodically, the Civis Data Science API is updated with new functionality. New functions can be generated to access these features automatically by reinstalling the `civis` package from CRAN.


API Documentation
-----------------

[https://platform.civisanalytics.com/api](https://platform.civisanalytics.com/api)


Contributing
------------
Contributions to the code are very welcome! Issues and bug reports can filed through the standard Github issue interface.  In order to submit a pull request, please fork the repo and make a pull request from the fork to the master branch of the main repo. Unit tests must pass before merging and writing tests for new code is highly encouraged!  Unit tests can be run with `Rscript tests/testthat.R`. For major, potentially breaking changes, integration tests should be run with
```bash
cd tools/integration_tests
Rscript smoke_test.R
```
Note, integration tests require a valid API key to run, and my be slow.

All contributors of code should include themselves in `DESCRIPTION` by adding
the following object in the `Authors@R` section:

```r
person("FirstName", "LastName", email = "email@email.com", role = "ctb")
```

This project is intended to be a safe, welcoming space for collaboration, and
contributors are expected to adhere to the [Contributor Covenant](http://contributor-covenant.org) code of conduct.
