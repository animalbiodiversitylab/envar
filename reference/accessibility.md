# Download and process Global Accessibility Indicators

This function downloads, processes, and extracts variables from the
Global Accessibility Indicators dataset. Each variable corresponds to a
global raster representing the travelling time (in minutes) to cities or
ports of specific sizes.

## Usage

``` r
accessibility(x, vars, ...)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.

- ...:

  Additional arguments (currently unused).

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**Available variables** (working synonyms in parentheses):

**Cities**

- 1 - "cities1" ("cities 1", "city 1", "cities \>5m", "huge cities",
  "travel time cities 1")

- 2 - "cities2" ("cities 2", "city 2", "cities \>1m", "large cities",
  "travel time cities 2")

- 3 - "cities3" ("cities 3", "city 3", "medium cities", "travel time
  cities 3")

- 4 - "cities4" ("cities 4", "city 4", "small cities", "travel time
  cities 4")

- 5 - "cities5" ("cities 5", "city 5", "travel time cities 5")

- 6 - "cities6" ("cities 6", "city 6", "travel time cities 6")

- 7 - "cities7" ("cities 7", "city 7", "travel time cities 7")

- 8 - "cities8" ("cities 8", "city 8", "towns", "travel time cities 8")

- 9 - "cities9" ("cities 9", "city 9", "small towns", "travel time
  cities 9")

- 10 - "cities10" ("cities 10", "city 10", "aggregated cities 1",
  "travel time cities 10")

- 11 - "cities11" ("cities 11", "city 11", "aggregated cities 2",
  "travel time cities 11")

- 12 - "cities12" ("cities 12", "city 12", "aggregated cities 3",
  "travel time cities 12")

**Ports**

- 13 - "ports1" ("ports 1", "port 1", "large ports", "travel time ports
  1")

- 14 - "ports2" ("ports 2", "port 2", "medium ports", "travel time ports
  2")

- 15 - "ports3" ("ports 3", "port 3", "small ports", "travel time ports
  3")

- 16 - "ports4" ("ports 4", "port 4", "very small ports", "travel time
  ports 4")

- 17 - "ports5" ("ports 5", "port 5", "any port", "all ports", "travel
  time ports 5")

**Citation:**  
Nelson A, Weiss DJ, van Etten J et al (2019). "A suite of global
accessibility indicators." Scientific Data 6, 266.
https://doi.org/10.1038/s41597-019-0265-5

Note: Data extent is \[-180, 180, -60, 85\].

## Examples

``` r
if (FALSE) { # \dontrun{
processed <- par_set(country= "Italy", crs=3035) %>% 
accessibility(vars=c("large cities", "ports1"))
  } # }
```
