# exuber 0.2.0

## Options

Some of the arguments in the functions have been as options to package, you can
easily set the options by e.g. `options(exuber.show_progress = TRUE)`.

* `parallel` option boolean, allows for parallel computing in critical values computation 
* `ncores` option numeric, sets the number of cores, defaults to max - 1
* `show_progress` option boolean, allows you to disable the progress bar, defaults to TRUE

## New features

* Default critical values are supplied directly into `summary()`, `diagnostics`,
  `datestamp()` and `autoplot()`, without having to specify argument cv. The 
  critical values have been simulated from `mc_cv()` function and stored as data,
* Panel estimation in `radf()`
* Added `sb_cv()` function: Sieve Bootstrapped critical values
* Added `ggarrange()` function, that can arrange a list of ggplot objects into a single grob

## Improvements

* Parallel and ncores arguments are now set as options
* Ability to remove progressbar from package options
* `radf()` can parse date from `ts` objects
* `report()` has been renamed into `summary()`
* `plot()` has been renamed into `autoplot()`, better synergy with autoplot

## Bug Fixes

* Progressbar appears directly when using 
* Plotiing date now works without having to to include any additonal plotting option
