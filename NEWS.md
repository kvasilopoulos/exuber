# exuber 0.4.0

## Design

We have the following design in mind for future scalability. If you want make inference about `radf` models, then the estimation can be achieved with `radf()` function and return an object of class `radf_obj`, and the critical values can be achieved with `radf_*_cv()` and return an object of class `radf_cv`.

## Breaking changes

* `autoplot()` for `radf` models has been refactored and new features have been added for more flexibility and conformity with the {ggplot} mindset.
* Because of the change in `autoplot`, `ggarrange()` is now defunct.
* `fortify()` methods have been replaced by `tidy()`, `augment()`, `tidy_join()` and `glance_join()` methods. `fortify()` methods are now defunct.
* Also `glance()` is now defunct. The user can use `tidy()` with `panel=TRUE` instead.
* Changed the names of:
  - `mc_cv()` to `radf_mc_cv()`. `mc_cv()` is now deprecated.
  - `mc_distr()` to `radf_mc_distr()`. `mc_distr()` is now deprecated.
  - `wb_cv()` to `radf_wb_cv()`. `wb_cv()` is now deprecated.
  - `wb_distr()` to `radf_wb_distr()`. `wb_distr()` is now deprecated.
  - `sb_cv()` to `radf_sb_cv()`. `sb_cv()` is now deprecated.
  - `sb_distr()` to `radf_sb_distr()`. `sb_distr()` is now deprecated.
  - `crit` dataset to `radf_crit`.
  - `col_names()` to `series_names()`.  `col_names()` is now deprecated.

## exuberdata

* We created a new package called `exuberdata` that accommodates critical values for up to 2000 observations. Critical values can be examined with `exuberdata::radf_crit2`. The package is created through `drat` R archive Template, and can be easily installed with `install.packages('exuberdata', repos = 'https://kvasilopoulos.github.io/drat/', type = 'source')` or through `install_exuberdata` wrapper function that is provided in `exuber`.

## Improvements

* The package `zoo` has been used as a dependency to import the method `index()`. 
We made the decision to remove `zoo` and create a new method `index()` internally.

# exuber 0.3.0

## Breaking changes

* Changed `opt_bsadf = conservative` for the simulated critical values (`crit`),
also reduced the size of the `crit` from 700 to 600 due to package size restrictions.
* `sim_dgp1()` and `sim_dgp2()` have been renamed to `sim_psy1()` and `sim_psy2()` 
to better describe the origination of the dgp. 
* `sim_dgp1()` and `sim_dgp2()` have been soft-deprecated.
* `autoplot_radf()` arranges automatically multiple graphs, to return to previous
behavior we included the optional argument `arrange` which is set to TRUE by default.

Three new functions have been added to simulate empirical distributions for:

* `mc_dist()`: Monte Carlo 
* `wb_dist()`: Wild Bootstrap 
* `sb_dist()`: Sieve Bootstrap 

and a function that can calculate the p-values `calc_pvalue()` given the above 
distributions as argument.

Also methods `tidy()` and `autoplot()` have been added to turn the object into
a tidy tibble and draw a particular plot with ggplot2, respectively.

## New features

* `tidy()` methods for objects of class `radf`, `cv`.
* `augment()` methods for objects of class `radf` and `cv`.
* `augment_join()` to combine object `radf` and `cv` into a single data.frame.
* `glance()` method for objects of class `radf`.

## Improvements

* New printing output for the functions `summary()`, `diagnostics()` and 
`datestamp()`.
* New improved progressbar with more succinct printing for `wb_cv()`
* `seed` argument to functions that are using rng. Also the option to declare
a global seed for reproducibility with the `option(exuber.global_seed = ###)`

## Bug Fixes

* `sb_cv()` and `wb_cv()`now can parse data that contain a date-column. Similarly,
to what `radf()` is doing.


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
