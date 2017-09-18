This submission fixes build issues:

- `configure` and `configure.win` now use `R_HOME` to find `Rscript` instead of assuming that `Rscript` is on the path.
- `configure.win` uses `Rscript.exe` for windows builds.
- `configure` uses `bin/sh` instead of `bin/bash`
- `configure.win` does not use `bin/bash`.

Unfortunately, we are unable to test the actual build environments, so we have to make repeated submissions until this works. Thank you for your patience.

The version has been bumped to 1.0.1.

## Test environments
* local OS X install, R 3.4.1
* ubuntu 12.04 (on travis-ci), R 3.4.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Patrick Miller <pmiller@civisanalytics.com>'
