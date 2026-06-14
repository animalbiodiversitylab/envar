# Download and process EarthEnv Topography layers

This function downloads, processes, and extracts variables from the
EarthEnv Topography dataset. This dataset provides global, cross-scale
topographic variables suitable for biodiversity and ecosystem modeling.

## Usage

``` r
topography(x, vars, algorithm = "md", topo_source = "GMTED", ...)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.

- algorithm:

  Character. The aggregation method/algorithm to use.

  - Common options: "md" (median, default), "mn" (mean), "min", "max",
    "sd".

  - Note: These codes directly affect the downloaded filename (e.g.,
    `_1KMmd_`).

- topo_source:

  Character. The source of the data.

  - "GMTED" (Global Multi-resolution Terrain Elevation Data) - Default.

  - "SRTM" (Shuttle Radar Topography Mission).

- ...:

  Additional arguments (currently unused).

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

**\[Topography Variables\]**

- 1 - "elevation" (\[m\]) ("dem", "height", "alt", "altitude")

- 2 - "slope" (\[degrees\])

- 3 - "aspect" (\[degrees\])

- 4 - "roughness" (\[Index\]) ("rough")

- 5 - "tri" (\[Index\]) ("terrain ruggedness index", "ruggedness")

- 6 - "tpi" (\[Index\]) ("topographic position index", "position")

- 7 - "vrm" (\[Index\]) ("vector ruggedness measure")

- 8 - "pcurv" (\[radians/m\]) ("profile curvature", "profile curve")

- 9 - "tcurv" (\[radians/m\]) ("tangential curvature", "tangential
  curve")

- 10 - "eastness" (\[Index\]) ("east")

- 11 - "northness" (\[Index\]) ("north")

**Citation:**  
Amatulli G, Domisch S, Tuanmu M-N, Parmentier B, Ranipeta A, Malczyk J,
Jetz W (2018). "A suite of global, cross-scale topographic variables for
environmental and biodiversity modeling." Scientific Data 5, 180040.
https://doi.org/10.1038/sdata.2018.40

Note: Please cite original sources of primary datasets where
appropriate.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Download elevation and slope for Italy
processed <- par_set(country = "Italy", crs = 3035) %>% 
  topography(vars = c("elevation", "slope"))
  } # }
```
