Civis API Client
================
[![Build Status](https://travis-ci.com/civisanalytics/civis-r-client.svg?token=E2j26hcJpSqCtyNqWd2B&branch=open-source)](https://travis-ci.com/civisanalytics/civis-r-client)

Setup
-----

1. Get a Civis API key [(instructions)](https://console.civisanalytics.com/support#/api). Instructions cannot be run on the Civis Platform if an API key is not found or if an API key is expired. By default, API keys expire after 30 days. Repeat these instructions when an expired API key must be updates.
2. Add a `CIVIS_API_KEY` environmental variable. It is best to put this in `.Renviron` to accommodate the many ways in which R can be run. When a key expires (usually after 30 days), the new key will need to be saved in `.Renviron`.
3. Alternatively, you may set the API key inside an R session:

```r
    Sys.setenv(CIVIS_API_KEY = "somestringwithlettersandnumbers")    
```

Installation
------------

```
git clone git@github.com:civisanalytics/civis-r.git
Rscript -e "devtools::install('civis-r');"
```

Usage
-------
```r
library(civis)

users_list_me()
```

Updating
--------
The `civis` package automatically generates R functions and documentation to interact with the Civis Platform when the package installs. Periodically, the Civis Platform API is updated with new functionality. New functions can be generated to access these features automatically by reinstalling the `civis` package from CRAN.


API Documentation
-----------------

[https://console.civisanalytics.com/support#/api](https://console.civisanalytics.com/support#/api)


Contributing
------------
Contributions to the code are very welcome! Issues and bug reports can filed through the standard Github issue interface.  In order to submit a pull request, please fork the repo and make a pull request from the fork to the master branch of the main repo. Unit tests must pass before merging and writing tests for new code is highly encouraged!  Unit tests can be run with `Rscript tests/testthat.R`. For major, potentially breaking changes, integration tests should be run with
```bash
cd tools/integration_tests
Rscript smoke_test.R
```
Note, integration tests require a valid Civis API key to run, and my be slow.

All contributors of code should include themselves in `DESCRIPTION` by adding
the following object in the `Authors@R` section:

```r
person("FirstName", "LastName", email = "email@email.com", role = "ctb")
```

This project is intended to be a safe, welcoming space for collaboration, and
contributors are expected to adhere to the [Contributor Covenant](http://contributor-covenant.org) code of conduct.
