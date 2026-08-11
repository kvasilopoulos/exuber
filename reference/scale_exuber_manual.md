# Exuber scale and theme functions

`scale_exuber_manual` allows specifying the color, size and linetype in
`autoplot.radf_obj` mappings. `theme_exuber` is a complete theme which
control all non-data display.

## Usage

``` r
scale_exuber_manual(
  color_values = c("red", "blue"),
  linetype_values = c(2, 1),
  size_values = c(0.8, 0.7)
)

theme_exuber(
  base_size = 11,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22
)
```

## Arguments

- color_values:

  a set of color values to map data values to.

- linetype_values:

  a set of linetype values to map data values to.

- size_values:

  a set of size values to map data values to.

- base_size:

  base font size, given in pts.

- base_family:

  base font family

- base_line_size:

  base size for line elements

- base_rect_size:

  base size for rect elements
