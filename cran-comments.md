## Test environments

* local OS MS install, R 3.4.5
* Continuous Integration
  * Ubuntu Trusty 14.04 on travis-ci (devel and release)
  * macOS on travis-ci (devel and release)
  * Windows Server 2012 on appveyor (devel and release)
* Rhub
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  * Debian Linux, R-devel, GCC ASAN/UBSAN
  * Fedora Linux, R-devel, clang, gfortran
  * Ubuntu Linux 16.04 LTS, R-release, GCC
* win-builder (devel, release and old_release)

## R CMD check results 

### devtools::check()  

There were no ERRORs and WARNINGs, with local checks or on remote checks.

0 errors √ | 0 warnings √ | 0 notes √
      
### rhub::check_for_cran() & devtools::check_win_*()

* checking CRAN incoming feasibility (26.4s) ... NOTE
Maintainer: 'Kostas Vasilopoulos <k.vasilopoulo@gmail.com>'
   
  - Uses the superseded package: 'doSNOW' 
  
  *(Use of the 'doSNOW' package as opposed to the 'doParallel' to support txtProgressBar)*
   
  - Possibly mis-spelled words in DESCRIPTION:
      Pavlidis (13:34)
     al (12:86, 13:46)
     et (12:83, 13:43)
  
  *(The names of the authors in the doi included in the DESCRIPTION)* 

## Reverse dependencies

There are no reverse dependencies.



