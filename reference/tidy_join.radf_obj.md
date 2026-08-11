# Tidy into a joint model

Tidy or augment and then join objects of class `radf_obj` and `radf_cv`.
The object of reference is the `radf_cv`. For example, if panel critical
values are provided the function will return the panel test statistic.

## Usage

``` r
# S3 method for class 'radf_obj'
tidy_join(x, y = NULL, ...)

# S3 method for class 'radf_obj'
augment_join(x, y = NULL, trunc = TRUE, ...)
```

## Arguments

- x:

  An object of class `radf_obj`.

- y:

  An object of class `radf_cv`. The output will depend on the type of
  critical value.

- ...:

  Further arguments passed to methods. Not used.

- trunc:

  Whether to remove the period of the minimum window from the plot
  (default = TRUE).

## Details

`tidy_join` also calls `augment_join` when `cv` is of class `sb_cv`.
