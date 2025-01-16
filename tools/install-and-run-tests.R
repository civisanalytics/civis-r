# This script is for running tests in Docker, in Travis CI or locally.
install.packages(c(
    "devtools",
    # Make sure we have a recent version of testthat.
    "testthat"
))
devtools::install_local(".", dependencies = TRUE)
devtools::check()
