# Benchmark example 08 - global extent, 3 sources, 1 layer each
#
# Source 1: CHELSA monthly time series (one variable / year / month)
# Source 2: EarthEnv topography
# Source 3: MELC land cover

library(envar)

env <- par_set(cache = TRUE) %>%
  chelsa(vars = "tas", years = 2019, months = 1) %>%
  topography(vars = "tpi") %>%
  melc(vars = "tree")

print(env)
