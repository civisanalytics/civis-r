## R CMD check results

0 errors | 2 warnings | 2 notes

* This is a new release.

* checking CRAN incoming feasibility ... NOTE
  * New maintainer: Peter Cooman <pcooman@civisanalytics.com>
  * Old maintainer: Patrick Miller <pmiller@civisanalytics.com>

* checking whether package ‘civis’ can be installed ... WARNING
  * `default_credential` masks `civis::default_credential()`.
  * `get_database_id` masks `civis::get_database_id()`.
  * `sql` masks `civis::sql()`.

  --> the civis:: functions overwrite themselves. There are no actual conflicts. 
  
* checking DESCRIPTION meta-information ... NOTE
  * Package listed in more than one of Depends, Imports, Suggests, Enhances:
  ‘future’

 --> 'future' is listed twice in the Imports section: once with a lower version bound and a second time with an upper version boud:
 ```
 Imports:
    future (>= 1.8.0),
    future (<= 1.31.0),
 ```
 The 'future' dependency does not appear in the other sections (Depends, Suggests or Enhances), so I'm not sure why this Note appeared. Please let me know if there is a better way to specify a valid version range for a dependency.
 
* checking top-level files ... WARNING
  * A complete check needs the 'checkbashisms' script.

  --> I believe this Warning only appears because I am running the checks on my local system. I did not see this Warning when I ran `devtools::check_rhub()` or `devtools::check_win_release()`