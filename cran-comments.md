## Test environments
* local OS MS install, R 3.4.5
* ubuntu 14.04 on travis-ci (R-oldrel and R-devel)
* macOS 12.06 on travis-ci (devel and release)
* Windows Server 2012 on appveyor (devel and release)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

There were no ERRORs, WARNINGs, or NOTEs with local checks or on Travis CI/Appveyor.

On devtools::release() R's CMD check we get one NOTE:

* N  checking CRAN incoming feasibility (1.9s)
   Maintainer: 'Vasilopoulos Kostas <kostasvasilo91@gmail.com>'
   
   Uses the superseded package: 'doSNOW'

We use doSNOW instead of parallel because doSNOW supports progress bar while doParallel does not.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.
