# Benchmark example 10 - global extent, 3 sources + extr_check()
#
# Same three layers as ex08, with the extrapolation check on top.
# Calibration points: all Apollo occurrences (western Palearctic).

library(envar)

env <- par_set(cache = TRUE) %>%
  chelsa(vars = "tas", years = 2019, months = 1) %>%
  topography(vars = "tpi") %>%
  melc(vars = "tree") %>%
  extr_check(calib_points = Apollo, calib_crs = 4326, type = "strict")

print(env$extrapolation)
