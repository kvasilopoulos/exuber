## Test environments

* local OS MS install, R 3.4.5
* Continuous Integration
  * ubuntu 14.04 on travis-ci (R-oldrel and R-devel)
  * macOS 12.06 on travis-ci (devel and release)
  * Windows Server 2012 on appveyor (devel and release)
* Rhub
  * Debian Linux, R-devel, GCC ASAN/UBSAN
  * Fedora Linux, R-devel, clang, gfortran
  * Ubuntu Linux 16.04 LTS, R-release, GCC
* win-builder (devel, release and old_release)

## R CMD check results 

### devtools::check()  

There were no ERRORs and WARNINGs, with local checks or on remote checks.

0 error | 0 warnings  | 1 note 

* checking installed package size ... NOTE
    installed size is 17.4Mb
    sub-directories of 1Mb or more:
      data  15.6Mb
      libs   1.6Mb
      
### rhub::check_cran()

* checking CRAN incoming feasibility (26.4s) ... NOTE
Maintainer: 'Kostas Vasilopoulos <k.vasilopoulo@gmail.com>'
   
  - Uses the superseded package: 'doSNOW' 
  
  *(Use of the 'doSNOW' package as opposed to the 'doParallel' to support txtProgressBar)*
   
  - Size of tarball: 21031594 bytes

  - Possibly mis-spelled words in DESCRIPTION:
      Shi (10:159)
      Yu (10:171)
  
  *(The names of the authors in the doi included in the DESCRIPTION)*    
  

## Reverse dependencies

There are no reverse dependencies.



