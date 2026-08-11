# Retrieve/Replace the index

Retrieve or replace the index of an object.

## Usage

``` r
index(x, ...)

index(x) <- value
```

## Arguments

- x:

  An object.

- ...:

  Further arguments passed to methods.

- value:

  An ordered vector of the same length as the `index` attribute of x.

## Details

If the user does not specify an index for the estimation a pseudo-index
is generated which is a sequential numeric series. After the estimation,
the user can use `index()` to retrieve or `index<-()` to replace the
index. The index can be either numeric or Date.
