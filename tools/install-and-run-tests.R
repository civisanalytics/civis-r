# This script is for running tests in Docker, in Travis CI or locally.
install.packages("devtools")
devtools::install_local(".", dependencies = TRUE)
devtools::check()
