
# exuber 0.2.1

* Changed DESCRIPTION to include `sb_cv` reference.
* Renamed boolean to dummy from `datestamp` and `diagnostics`.
* `datestamp` dummy is now an attribute.

# exuber 0.2.0

## Options

Some of the arguments in the functions were included as options, you can
set the package options with e.g. `options(exuber.show_progress = TRUE)`.

* `parallel` option boolean, allows for parallel in critical values computation.
* `ncores` option numeric, sets the number of cores, defaults to max - 1.
* `show_progress` option boolean, allows you to disable the progress bar, defaults to TRUE.

## New features

* Panel estimation in `radf()`
* Added `sb_cv()` function: Panel Sieve Bootstrapped critical values
* Default critical values are supplied directly into `summary()`, `diagnostics`,
  `datestamp()` and `autoplot()`, without having to specify argument cv. The 
  critical values have been simulated from `mc_cv()` function and stored as data.
  Custom critical values should be provided by the user with the option `cv`.
* Added `ggarrange()` function, that can arrange a list of ggplot objects into a single grob.
* Added `fortify` to arrange a data.frame from `radf()` function.

## Improvements

* Parallel and ncores arguments are now set as options.
* Ability to remove progressbar from package options.
* `radf()` can parse date from `ts` objects.
* `report()` has been renamed into `summary()`.
* `plot()` has been renamed into `autoplot()`.
* `plot()` and `report()` are soft deprecated.

## Bug Fixes

* Progressbar appears in the beginning of the iteration
* Plotting date now works without having to to include any additional plotting option
