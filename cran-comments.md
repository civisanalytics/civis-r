## R CMD check results

0 errors | 2 warnings | 1 note

* This is a new release.

* checking CRAN incoming feasibility ... NOTE
  * Maintainer: Peter Cooman <pcooman@civisanalytics.com>
  Days since last update: 5

  --> This patch update provides a more permanent fix to planned changes in an upcoming 
  version of the `future` package that would otherwise break our package. We do not 
  anticipate needing to make any other changes in the near future.
  
* checking whether package ‘civis’ can be installed ... WARNING
  * `default_credential` masks `civis::default_credential()`.
  * `get_database_id` masks `civis::get_database_id()`.
  * `sql` masks `civis::sql()`.

  --> the civis:: functions overwrite themselves. There are no actual conflicts. 
  
* checking top-level files ... WARNING
  * A complete check needs the 'checkbashisms' script.

  --> I believe this Warning only appears because I am running the checks on my local system. I did not see this Warning when I ran `devtools::check_rhub()` or `devtools::check_win_release()`