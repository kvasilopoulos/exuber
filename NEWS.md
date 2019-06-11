# exuber 0.3.0

Three new functions have been added to simulate emprical distributions for:

* `mc_dist()`: Monte Carlo distribution
* `wb_dist()`: Wild Bootsrap distribution
* `sb_dist()`: Sieve Bootstrap distribution

Also methods `tidy()` and `autoplot()` have been added to turn the object into
a tidy tibble and draw a particular plot with ggplot2, respectively.

## Breaking changes

* `sim_dgp1()` and `sim_dgp2()` have been renamed to `sim_psy1()` and `sim_psy2()` 
to better describe the origination of the dgp. 
* `sim_dgp1()` and `sim_dgp2()` have been soft-deprecated.
* `autoplot_radf()` arranges automatically multiple graphs, to return to previous
behavior we included the optional argument `arrange` which is set to TRUE by default.

## New features

* `tidy()` methods for objects of class `radf`, `cv`.
* `augment()` methods for objects of class `radf` and `cv` 
* `glance()` method for objects of class `radf`

## Impovements

* New printing output for the functrions `summary()`, `diagnostics()` and 
`datestamp()`.
* New improved progressbar with more succinct printing for `wb_cv()`

## Bug Fixes

* `sb_cv()` and `wb_cv()`now can parse data which contain a date-column. Similarly,
to what `radf()` is doing, but without keeping the index.


# exuber 0.2.1.9000

* Website development

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
