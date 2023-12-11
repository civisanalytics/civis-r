Civis Platform API R Client
================
[![Build Status](https://travis-ci.org/civisanalytics/civis-r.svg?branch=master)](https://travis-ci.org/civisanalytics/civis-r)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/civis)](https://cran.r-project.org/package=civis)

## Introduction

`civis` is an R package that helps analysts and developers interact with
the Civis Platform. The package includes a set of tools around common
workflows as well as a convenient interface to make requests directly to
the Civis Platform API.


## Documentation

The full documentation is hosted [here](https://civisanalytics.github.io/civis-r/). The fastest way to get started is with the [getting started guide](https://civisanalytics.github.io/civis-r/articles/quick_start.html).

## API Keys

Installing and using `civis` requires an API key.  Instructions
for creating an API key can be found [here](https://civis.zendesk.com/hc/en-us/articles/216341583-Generating-an-API-Key).
All API keys have a set expiration date and so a new key will need to be
created at least every 30 days.

Once you have created an API key, you will then need to add it to your
`.Renviron` so the `civis` package can access it.

#### Linux/MacOS

```bash
touch ~/.Renviron
open -t ~/.Renviron
```

Then add the following line replacing the fake key `sadf8sadf9jasdf` with the API key from your Civis Platform Profile:

```
CIVIS_API_KEY=sadf8sadf9jasdf
```
After saving the `.Renviron` file, you'll need to restart R/Rstudio.

#### Windows

A `.Renviron` file can be created in the R user home directory, `Sys.getenv("R_USER")`. Typically this is `"C:/username/Documents"`. Open or create `.Renviron` with `notepad` and add the key as above. Save with type `all files`, not `.txt`. Restart R/Rstudio.

:heavy_exclamation_mark: You must keep your API key secret. If you use version
control tools, ensure you do not commit `.Renviron` or any scripts in which
you have hard-coded an API key.


## Install

```r
# Default install
install.packages("civis")

# All features (including CivisML)
install.packages("civis", depends = TRUE)
```

### Updating

The `civis` package can automatically generate R functions and documentation to interact with the Civis Platform when the package installs.

From time to time, new API functions are added. These can be obtained immediately by installing the package from source:

```r
install.packages('civis', depends = TRUE, type = 'source')
```

## Usage

`civis` includes functionality for both

1. Making single calls to accomplish a specific task (which may involve
making multiple calls to the API)
2. Making direct calls to the API

Many useful tasks will require making multiple direct calls to the API.
`civis` includes a number of wrapper functions to make common tasks like IO
and modeling easier. For example, reading data from a table in
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

### Direct calls to API

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

## API Documentation

[https://platform.civisanalytics.com/#/api](https://platform.civisanalytics.com/#/api)


## Contributing

Contributions to the code are very welcome! Issues and bug reports can filed through the standard Github issue interface.

### git-flow

1. Fork the repo, make changes in your branch.
2. Unit tests must pass, and can be run with `devtools::test()` or `cmd-shift-t` in Rstudio.
3. New code must be backed with tests, mocking all API calls.
4. `R CMD check` must pass, i.e. using `devtools::check()`.
5. Make a pull request from the fork to the master branch of the main repo. In your pull request, link to the issue the pull request addresses.
6. Tag a maintainer for review. Once it has been approved, squash and merge!

Adding the environment variable `R_CLIENT_DEV = "TRUE"` to `.Renviron` will prevent new API functions and documentation from being generated based on your API Key. The default generated API functions included in the package are only updated when this package releases to CRAN.

### Integration tests

For major, potentially breaking changes, integration tests should be run with
```bash
cd tools/integration_tests
Rscript smoke_test.R
```
Note, integration tests require a valid API key to run, and my be slow.


### Authorship

All contributors of code should include themselves in `DESCRIPTION` by adding
the following object in the `Authors@R` section:

```r
person("FirstName", "LastName", email = "email@email.com", role = "ctb")
```

This project is intended to be a safe, welcoming space for collaboration, and
contributors are expected to adhere to the [Contributor Covenant](https://www.contributor-covenant.org/) code of conduct.
