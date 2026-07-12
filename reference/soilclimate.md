# Download and process Soil Bioclimatic and Temperature layers

This function downloads, processes, and extracts soil bioclimatic
variables and monthly soil temperatures from the Global Soil Temperature
dataset (Lembrechts et al., 2022).

## Usage

``` r
soilclimate(x, vars, depth = "0-5", ...)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.

- depth:

  Character string defining the soil depth range. Options are "0-5"
  (default) or "5-15".

- ...:

  Additional arguments (currently unused).

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

**Bioclimatic Variables**

- SBIO1 - "SBIO1" \[°C\] ("annual mean temperature", "annual mean",
  "amt")

- SBIO2 - "SBIO2" \[°C\] ("mean diurnal range", "mean diurnal", "mdr")

- SBIO3 - "SBIO3" \[Ratio\] ("isothermality")

- SBIO4 - "SBIO4" \[SD\] ("temperature seasonality", "seasonality")

- SBIO5 - "SBIO5" \[°C\] ("max temperature warmest month", "max temp",
  "warmest month")

- SBIO6 - "SBIO6" \[°C\] ("min temperature coldest month", "min temp",
  "coldest month")

- SBIO7 - "SBIO7" \[°C\] ("temperature annual range", "annual range",
  "tar")

- SBIO8 - "SBIO8" \[°C\] ("mean temperature wettest quarter", "wettest
  quarter")

- SBIO9 - "SBIO9" \[°C\] ("mean temperature driest quarter", "driest
  quarter")

- SBIO10 - "SBIO10" \[°C\] ("mean temperature warmest quarter", "warmest
  quarter")

- SBIO11 - "SBIO11" \[°C\] ("mean temperature coldest quarter", "coldest
  quarter")

**Monthly Mean Soil Temperatures**

- soilT01 - "soilT01" \[°C\] ("january mean", "january", "jan")

- soilT02 - "soilT02" \[°C\] ("february mean", "february", "feb")

- soilT03 - "soilT03" \[°C\] ("march mean", "march", "mar")

- soilT04 - "soilT04" \[°C\] ("april mean", "april", "apr")

- soilT05 - "soilT05" \[°C\] ("may mean", "may")

- soilT06 - "soilT06" \[°C\] ("june mean", "june", "jun")

- soilT07 - "soilT07" \[°C\] ("july mean", "july", "jul")

- soilT08 - "soilT08" \[°C\] ("august mean", "august", "aug")

- soilT09 - "soilT09" \[°C\] ("september mean", "september", "sep")

- soilT10 - "soilT10" \[°C\] ("october mean", "october", "oct")

- soilT11 - "soilT11" \[°C\] ("november mean", "november", "nov")

- soilT12 - "soilT12" \[°C\] ("december mean", "december", "dec")

**Citation:**  
Lembrechts JJ et al. (2022)."Global maps of soil temperature." Global
Change Biology 28, 3110-3144.
[doi:10.1111/gcb.16060](https://doi.org/10.1111/gcb.16060)

Note: Please cite original sources of primary datasets where
appropriate.

## Examples

``` r
# \donttest{
processed <- par_set(country= "Italy", crs=3035) %>% 
soilclimate(vars=c("SBIO1", "SBIO10"), depth="5-15")
   # }
```
