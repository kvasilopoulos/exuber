# Augment into a joint model

Augment and then join objects.

## Usage

``` r
augment_join(x, y, ...)
```

## Arguments

- x:

  An object of class `obj`.

- y:

  An object of class `cv`.

- ...:

  Further arguments passed to methods.

## Value

A `tibble` joining the augmented statistics of `x` with the augmented
critical values of `y`; see the methods for the exact columns.
