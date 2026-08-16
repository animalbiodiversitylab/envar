# Benchmark example 11 - global extent aggregated to ~5 km, 3 sources
#
# Same three sources as ex08, but with res = 5, i.e. the ~1 km source grid
# aggregated by a factor of 5. This is the practical way to work globally.

library(envar)

env <- par_set(res = 5, cache = TRUE) %>%
  chelsa(vars = "tas", years = 2019, months = 1) %>%
  topography(vars = "tpi") %>%
  melc(vars = "tree")

print(env)
