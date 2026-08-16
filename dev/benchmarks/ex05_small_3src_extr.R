# Benchmark example 05 - small extent (Switzerland), 3 sources + extr_check()
#
# Same three layers as ex03, with the extrapolation check on top.
# Calibration points: the Apollo occurrences that fall inside Switzerland.

library(envar)

calib <- subset(Apollo, X >= 5.9 & X <= 10.5 & Y >= 45.8 & Y <= 47.8)

env <- par_set(country = "Switzerland", cache = TRUE) %>%
  chelsa(vars = "tas", years = 2019, months = 1) %>%
  topography(vars = "tpi") %>%
  melc(vars = "tree") %>%
  extr_check(calib_points = calib, calib_crs = 4326, type = "strict")

print(env$extrapolation)
