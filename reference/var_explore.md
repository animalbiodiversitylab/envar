# Explore available variables and parameters

Explore available variables and parameters

## Usage

``` r
var_explore(source = NULL, what = "sources", error_context = NULL)
```

## Arguments

- source:

  Data source to explore

- what:

  What to explore ("sources", "variables", "resolutions")

- error_context:

  Error context from var_get (if called from error handler)

## Value

Prints helpful information

## Examples

``` r
# Show available sources
var_explore()
#> 
#> ── Available data sources: ──
#> 
#> 
#> 
#> ── Climate Data: 
#> • worldclim: WorldClim 2.1 climate data
#> • chelsa: CHELSA climatologies v2.1
#> • chelsa_cmip5: CHELSA CMIP5 future projections
#> • chelsa_bioclimplus: Extended bioclimatic variables
#> • climate_stability: Climate stability indices
#> 
#> ── Land Cover: 
#> • esa_landcover: ESA Land Cover CCI
#> • consensus_landcover: Consensus land cover (Tuanmu & Jetz 2014)
#> 
#> ── Terrain & Cloud: 
#> • topography: Elevation, slope, aspect, roughness, TRI, TPI
#> • cloud: Cloud cover frequency from MODIS
#> 
#> ── Vegetation: 
#> • ndvi: NDVI time series from MODIS/GIMMS
#> • heterogeneity: Spectral heterogeneity metrics
#> 
#> ── Water & Drought: 
#> • spectre: SPEI, SPI, PDSI drought indices
#> • freshwater: HydroSHEDS hydrological variables
#> • aridity: Global Aridity Index and PET
#> 
#> ── Other Environmental: 
#> • hwsd: Harmonized World Soil Database
#> • wind: Global Wind Atlas

# Show variables for WorldClim
var_explore(source = "worldclim", what = "variables")
#> 
#> ── Available variables for "worldclim": ──
#> 
#> • bioclim: 19 bioclimatic variables (bio1-bio19)
#> • tmean: Monthly mean temperature
#> • tmin: Monthly minimum temperature
#> • tmax: Monthly maximum temperature
#> • prec: Monthly precipitation
#> • srad: Solar radiation
#> • wind: Wind speed
#> • vapr: Water vapor pressure
```
