# Download and process Global Simulation Land Use/Cover (GEOSOS) 1 km variables

This function downloads, processes, and extracts variables from the
Global Land-Use and Land-Cover Change Product (2010-2100). The dataset
provides global 1 km resolution rasters based on different IPCC
scenarios.

## Usage

``` r
geososlandcover(x, vars, scenario = "A1B", year = 2010, discover = TRUE, ...)
```

## Arguments

- x:

  The output from \`var_get()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`var_get()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.

- scenario:

  Character. The IPCC scenario: "A1B", "A2", "B1", or "B2". Ignored if
  year is 2010.

- year:

  Numeric or character. The year of the product: 2010, 2050, or 2100.

- ...:

  Additional arguments (currently unused).

## Value

If \`var_get()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`var_get()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

**Land Cover Classification**

- 1 - "landcover" (Categorical: 1=Water, 2=Forest, 3=Grassland,
  4=Farmland, 5=Urban, 6=Barren) ("lc", "cover", "land cover", "land
  cover class", "land use", "lulc", "classes")

**Simulation Parameters**

- **Years:** 2010, 2050, 2100 (Note: 2010 is the MODIS baseline).

- **Scenarios:** "A1B", "A2", "B1", "B2" (Ignored if year is 2010).

**Citation:**  
Li X, Chen G, Liu X, Liang X, Wang S, Chen Y, Pei F, Xu X (2017). "A new
global land-use and land-cover change product at a 1-km resolution for
2010 to 2100 based on human–environment interactions." Annals of the
American Association of Geographers 107(5), 1040–1059.
https://doi.org/10.1080/24694452.2017.1303357

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Download land cover for Italy in 2050 under scenario A1B
processed <- var_get(country = "Italy", crs = 3035) %>% 
  geososlandcover(vars = c("land cover"), year = 2050, scenario = "A1B")
  
# Example 2: Extract baseline (2010) values for specific points
points_df <- data.frame(ID = 1:2, x = c(12, 13), y = c(42, 43))
extracted <- var_get(data = points_df, crs = 4326) %>%
  geososlandcover(vars = "lc", year = 2010)
} # }
```
