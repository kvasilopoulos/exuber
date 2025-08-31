
## Test environments

* local OS MS install, R 4.3.2
* Continuous Integration
  * GitHub actions (ubuntu-20.04): release, devel
  * GitHub actions (windows): release
  * Github actions (OS X): release
* win-builder (devel)
* win-builder (release)

## R CMD check results 

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
  
## Reverse dependencies

There are no reverse dependencies.



