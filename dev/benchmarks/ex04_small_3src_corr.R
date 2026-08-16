# Benchmark example 04 - small extent (Switzerland), 3 sources + corr_check()
#
# Same three layers as ex03, with the collinearity check on top.

library(envar)

env <- par_set(country = "Switzerland", cache = TRUE) %>%
  chelsa(vars = "tas", years = 2019, months = 1) %>%
  topography(vars = "tpi") %>%
  melc(vars = "tree") %>%
  corr_check(pearson = 0.7, vif = 3)

print(env$vif)
