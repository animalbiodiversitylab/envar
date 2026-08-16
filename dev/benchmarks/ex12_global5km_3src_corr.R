# Benchmark example 12 - global extent at ~5 km, 3 sources + corr_check()

library(envar)

env <- par_set(res = 5, cache = TRUE) %>%
  chelsa(vars = "tas", years = 2019, months = 1) %>%
  topography(vars = "tpi") %>%
  melc(vars = "tree") %>%
  corr_check(pearson = 0.7, vif = 3)

print(env$vif)
