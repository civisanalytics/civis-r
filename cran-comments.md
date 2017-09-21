This submission fixes build issues:

- roxygen2::roxygenize, devtools::build, and memoise::memoise included in NAMESPACE
- Failing test should be less platform dependent. 

The version has been bumped to 1.0.2.

## Test environments
* local OS X install, R 3.4.1
* ubuntu 12.04 (oldrel, release, devel)
* win-builder (oldrel, release, and devel)

## R CMD check results
There were no ERRORs or WARNINGs.
