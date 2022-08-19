
## Test environments

* local OS MS install, R 4.0.3
* Continuous Integration
  * GitHub actions (ubuntu-20.04): release, devel
  * GitHub actions (windows): release
  * Github actions (OS X): release
* Rhub
  * Debian Linux, R-devel, GCC ASAN/UBSAN
  * Fedora Linux, R-devel, clang, gfortran
* win-builder (devel)

## R CMD check results 

   
### rhub::check_for_cran() & devtools::check_win_devel() & rcmdcheck::rcmdcheck(env = c("_R_CHECK_RD_VALIDATE_RD2HTML_" = "TRUE"))

0 ERRORS √ | 0 WARNINGS √ | 2 NOTEs

```r
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Kostas Vasilopoulos <k.vasilopoulo@gmail.com>’

Suggests or Enhances not in mainstream repositories:
  exuberdata
Availability using Additional_repositories specification:
  exuberdata   yes   https://kvasilopoulos.github.io/drat

Uses the superseded package: ‘doSNOW (>= 1.0.16)’
```
  **(Use of the 'doSNOW' package as opposed to the 'doParallel' to support txtProgressBar)**
  
```r
Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.18637/jss.v103.i10
    From: man/exuber.Rd
    Status: 404
    Message: Not Found
```
**The DOI in the CITATION is for a new JSS publication that will be registered after publication on CRAN.**

## Reverse dependencies

There are no reverse dependencies.



