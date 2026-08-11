# Create a complete ggplot appropriate to a particular data type

`autoplot2()` uses ggplot2 to draw a particular plot for an object of a
particular class in a single command. This defines the S3 generic that
other classes and packages can extend.

## Usage

``` r
autoplot2(object, ...)
```

## Arguments

- object:

  an object, whose class will determine the behaviour of autoplot

- ...:

  other arguments passed to specific methods

## See also

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
