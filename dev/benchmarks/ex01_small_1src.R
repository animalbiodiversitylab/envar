# Benchmark example 01 - small extent (Switzerland), 1 source, 1 layer
#
# Source 1: CHELSA monthly time series, one variable / one year / one month.

library(envar)

env <- par_set(country = "Switzerland", cache = TRUE) %>%
  chelsa(vars = "tas", years = 2019, months = 1)

print(env)
