# Pipe operator

Re-exported from dplyr. See
`dplyr::`[`%>%`](https://dplyr.tidyverse.org/reference/reexports.html)
for details. This lets users chain `envar` functions, e.g.
`par_set(country = "Italy") %>% worldclim(vars = "bio1")`.

## Usage

``` r
lhs %>% rhs
```

## Arguments

- lhs:

  A value or the magrittr placeholder.

- rhs:

  A function call using the magrittr semantics.

## Value

The result of calling `rhs(lhs)`.
