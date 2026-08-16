# Benchmark example 09 - global extent, 3 sources + corr_check()
#
# Same three layers as ex08, with the collinearity check on top.

library(envar)

env <- par_set(cache = TRUE) %>%
  chelsa(vars = "tas", years = 2019, months = 1) %>%
  topography(vars = "tpi") %>%
  melc(vars = "tree") %>%
  corr_check(pearson = 0.7, vif = 3)

print(env$vif)
