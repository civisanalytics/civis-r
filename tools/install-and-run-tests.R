# This script is for running tests in Docker, in Travis CI or locally.
install.packages("devtools")
devtools::install_local(".", dependencies = TRUE)
# Run the tests separately from the check command so that warnings are logged.
devtools::test()
devtools::check(args = "--no-tests")
