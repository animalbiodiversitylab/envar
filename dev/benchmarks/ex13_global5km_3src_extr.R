# Benchmark example 13 - global extent at ~5 km, 3 sources + extr_check()

library(envar)

env <- par_set(res = 5, cache = TRUE) %>%
  chelsa(vars = "tas", years = 2019, months = 1) %>%
  topography(vars = "tpi") %>%
  melc(vars = "tree") %>%
  extr_check(calib_points = Apollo, calib_crs = 4326, type = "strict")

print(env$extrapolation)
