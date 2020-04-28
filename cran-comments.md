## Test environments

* local OS MS install, R 4.0.0
* Continuous Integration
  * GitHub actions (ubuntu-16.04): release
  * GitHub actions (windows): release
  * Github actions (OS X): release, devel
* Rhub
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  * Debian Linux, R-devel, GCC ASAN/UBSAN
  * Fedora Linux, R-devel, clang, gfortran
* win-builder (devel)

## R CMD check results 

### devtools::check()  

There were no ERRORs and WARNINGs, with local checks or on remote checks.

0 errors √ | 0 warnings √ | 0 notes √
      
### rhub::check_for_cran() & devtools::check_win_devel()

0 ERRORS √ | 0 WARNINGS √ | 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Kostas Vasilopoulos <k.vasilopoulo@gmail.com>’

Suggests or Enhances not in mainstream repositories:
  exuberdata
Availability using Additional_repositories specification:
  exuberdata   yes   https://kvasilopoulos.github.io/drat

Uses the superseded package: ‘doSNOW (>= 1.0.16)’

  **(Use of the 'doSNOW' package as opposed to the 'doParallel' to support txtProgressBar)**
  
* checking package dependencies ... NOTE
Package suggested but not available for checking: 'exuberdata'

## Reverse dependencies

There are no reverse dependencies.



