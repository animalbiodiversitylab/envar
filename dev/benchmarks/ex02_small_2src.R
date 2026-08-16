# Benchmark example 02 - small extent (Switzerland), 2 sources, 1 layer each
#
# Source 1: CHELSA monthly time series (one variable / year / month)
# Source 2: EarthEnv topography

library(envar)

env <- par_set(country = "Switzerland", cache = TRUE) %>%
  chelsa(vars = "tas", years = 2019, months = 1) %>%
  topography(vars = "tpi")

print(env)
