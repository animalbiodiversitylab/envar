# Download CHELSA climate data

This function downloads, processes, and extracts variables from the
CHELSA (Climatologies at High Resolution for the Earth's Land Surface
Areas) dataset.

## Usage

``` r
chelsa(
  x,
  vars,
  years = NULL,
  months = NULL,
  gcm = NULL,
  rcp = NULL,
  ssp = NULL,
  cruts_years = NULL,
  ...
)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.

- years:

  A character or numeric vector of years or year ranges (e.g.,
  "1981-2010", 2015).

- months:

  A numeric vector (1–12) specifying which months to download. If NULL
  and \`years\` are single years, all 12 months are downloaded.

- gcm:

  General Circulation Model(s) for future projections.

- rcp:

  Representative Concentration Pathway, given as the radiative-forcing
  level (e.g., `2.6`, `4.5`, `6.0`, `8.5`). For CMIP5 projections (year
  ranges `"2041-2060"`, `"2061-2080"`) it selects the RCP directly. For
  CMIP6/BIOCLIM+ projections it is combined with `ssp` to build the
  scenario code (e.g., `ssp = 5` and `rcp = 8.5` request the `ssp585`
  scenario).

- ssp:

  Shared Socioeconomic Pathway family for CMIP6/BIOCLIM+ data (e.g.,
  `1`, `2`, `3`, `5`). Combined with `rcp` as described above. A
  complete code such as `"585"` may also be supplied directly (with
  `rcp = NULL`).

- cruts_years:

  Numeric vector. Years to download from CHELSAcruts (must be
  1901–2016).

- ...:

  Additional arguments (currently unused).

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables**

Please note the distinction between "Monthly" time-series data and
"Climatologies". Unlike other functions in this package, there is only
one code-name for each variable and no working synonyms. The meaning of
each variable code-name is provided in parentheses.

**Monthly Time-Series (Available from 1979 onwards)**

- 1 - "pr" (Precipitation amount; mass per unit area)

- 2 - "tas" (Mean daily air temperature at 2 meters)

- 3 - "tasmax" (Mean daily maximum air temperature at 2 meters)

- 4 - "tasmin" (Mean daily minimum air temperature at 2 meters)

- 5 - "hurs" (Near-surface relative humidity)

- 6 - "clt" (Total cloud cover at surface; considers entire atmospheric
  column)

- 7 - "sfcWind" (Near-surface wind speed at 10m above ground)

- 8 - "vpd" (Vapor pressure deficit)

- 9 - "rsds" (Surface downwelling shortwave flux in air)

- 10 - "pet_penman" (Potential evapotranspiration; Penman-Monteith
  equation)

- 11 - "cmi" (Climate Moisture Index)

- 12 - "swb" (Site water balance; cumulative available water)

**Climatologies & Derived Indices (1981-2010, 2011-2040, 2041-2070,
2071-2100)**

**Cloud Cover**

- 13 - "clt_mean" (Mean monthly total cloud cover over 1 year)

- 14 - "clt_max" (Maximum monthly total cloud cover)

- 15 - "clt_min" (Minimum monthly total cloud cover)

- 16 - "clt_range" (Annual range of monthly total cloud cover)

**Climate Moisture Index**

- 17 - "cmi_mean" (Mean monthly climate moisture index)

- 18 - "cmi_max" (Maximum monthly climate moisture index; highest
  surplus)

- 19 - "cmi_min" (Minimum monthly climate moisture index; highest
  deficit)

- 20 - "cmi_range" (Annual range of monthly climate moisture index)

**Relative Humidity**

- 21 - "hurs_mean" (Mean monthly near-surface relative humidity)

- 22 - "hurs_max" (Maximum monthly near-surface relative humidity)

- 23 - "hurs_min" (Minimum monthly near-surface relative humidity)

- 24 - "hurs_range" (Annual range of monthly near-surface relative
  humidity)

**Potential Evapotranspiration**

- 25 - "pet_penman_mean" (Mean monthly PET)

- 26 - "pet_penman_max" (Maximum monthly PET)

- 27 - "pet_penman_min" (Minimum monthly PET)

- 28 - "pet_penman_range" (Annual range of monthly PET)

**Solar Radiation**

- 29 - "rsds_mean" (Mean monthly surface downwelling shortwave flux)

- 30 - "rsds_max" (Maximum monthly surface downwelling shortwave flux)

- 31 - "rsds_min" (Minimum monthly surface downwelling shortwave flux)

- 32 - "rsds_range" (Annual range of monthly surface downwelling
  shortwave flux)

**Wind Speed**

- 33 - "sfcWind_mean" (Mean monthly near-surface wind speed)

- 34 - "sfcWind_max" (Maximum monthly near-surface wind speed)

- 35 - "sfcWind_min" (Minimum monthly near-surface wind speed)

- 36 - "sfcWind_range" (Annual range of monthly near-surface wind speed)

**Vapor Pressure Deficit**

- 37 - "vpd_mean" (Mean monthly vapor pressure deficit)

- 38 - "vpd_max" (Maximum monthly vapor pressure deficit)

- 39 - "vpd_min" (Minimum monthly vapor pressure deficit)

- 40 - "vpd_range" (Annual range of monthly vapor pressure deficit)

**Growing Season Characteristics (TREELIM model)**

- 41 - "gsl" (Growing season length; days)

- 42 - "gsp" (Accumulated precipitation during growing season)

- 43 - "gst" (Mean temperature of the growing season)

- 44 - "fgd" (First day of the growing season; Julian day)

- 45 - "lgd" (Last day of the growing season; Julian day)

**Growing Degree Days (GDD)**

- 46 - "gdd0" (Heat sum of all days \> 0°C accumulated over 1 year)

- 47 - "gdd5" (Heat sum of all days \> 5°C accumulated over 1 year)

- 48 - "gdd10" (Heat sum of all days \> 10°C accumulated over 1 year)

- 49 - "ngd0" (Number of days with tas \> 0°C)

- 50 - "ngd5" (Number of days with tas \> 5°C)

- 51 - "ngd10" (Number of days with tas \> 10°C)

- 52 - "gdgfgd0" (First growing degree day \> 0°C; Julian day)

- 53 - "gdgfgd5" (First growing degree day \> 5°C; Julian day)

- 54 - "gdgfgd10" (First growing degree day \> 10°C; Julian day)

- 55 - "gddlgd0" (Last growing degree day \> 0°C; Julian day)

- 56 - "gddlgd5" (Last growing degree day \> 5°C; Julian day)

- 57 - "gddlgd10" (Last growing degree day \> 10°C; Julian day)

**Snow and Frost**

- 58 - "scd" (Snow cover days; count)

- 59 - "swe" (Snow water equivalent; accumulated amount of liquid water
  if snow melted)

- 60 - "fcf" (Frost change frequency; events where tmin/tmax cross 0°C)

**Biological Productivity**

- 61 - "npp" (Net primary productivity; g C m^-2 yr^-1)

**Climate Classifications**

- 62 - "kg0" (Köppen-Geiger climate category)

- 63 - "kg1" (Köppen-Geiger without As/Aw differentiation)

- 64 - "kg2" (Köppen-Geiger after Peel et al. 2007)

- 65 - "kg3" (Wissmann 1939 classification)

- 66 - "kg4" (Thornthwaite 1931 classification)

- 67 - "kg5" (Troll-Pfaffen classification)

**Citation:**  
Standard bioclimatic variables: Karger D, Conrad O, Böhner J et al
(2017). "Climatologies at high resolution for the earth’s land surface
areas." Scientific Data 4, 170122.
https://doi.org/10.1038/sdata.2017.122  

BIOCLIM+ dataset: Brun P, Zimmermann NE, Hari C, Pellissier L, Karger DN
(2022). "Global climate-related predictors at kilometer resolution for
the past and future." Earth System Science Data 14, 5573-5603.
https://doi.org/10.5194/essd-14-5573-2022

Note: Users should verify the terms of use for CHELSA data provided at
https://chelsa-climate.org/

## Examples

``` r
if (FALSE) { # \dontrun{

# climatic values for one specific year/month
processed <- par_set(zooregion = "Madagascan") %>%
chelsa(vars=c("tas"), years = 2018, months = 1)

# climatic values for a long period (real "climate"), if months are not specified
# all the months are downloaded (12 layers per variable)
processed <- par_set(country = "Iceland") %>%
chelsa(vars=c("pr", "tas"), years = "1981-2010", months = 1)

# bioclimatic variables are available only over these extended periods, and not
# for the single years
processed <- par_set(country = "Iceland") %>%
chelsa(vars=c("bio1"), years = "1981-2010", months = 1)

# to download a specified set of variables, leave only "bio" and then the
# package will ask which variables to download (all 19 or a selection) in the console
processed <- par_set(country = "Iceland") %>%
chelsa(vars=c("bio"), years = "1981-2010", months = 1)

# climatic values for the future (SSP, RCP and GCM must be specified)
processed <- par_set(country = "Italy", crs = 3035) %>%
chelsa(vars=c("pr", "tas"), years = "2041-2070", months = 1,
 ssp = 5, rcp = 8.5, gcm = "GFDL-ESM4")
   } # }
```
