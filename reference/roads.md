# Download and process Global Road Density layers

This function downloads, processes, and extracts road density variables.
Each variable corresponds to a global raster (~1 km resolution)
reporting the total length of roads (in metres) within each grid cell,
for a single road class or for a group of classes.

## Usage

``` r
roads(x, vars = "all", ...)
```

## Arguments

- x:

  The output from \`par_set()\` defining the area or locations for
  extraction, the reference system, and the buffer. Leave this empty and
  use \`par_set()\` to define parameters for download.

- vars:

  Character vector of one or more variables to download and process.
  Defaults to "all" (all road classes combined).

- ...:

  Additional arguments (currently unused).

## Value

If \`par_set()\` contained a raster/polygon/points with buffer: a
\`SpatRaster\` stack of processed variables. If \`par_set()\` contained
spatial points or data.frame of points without buffer: a \`data.frame\`
of x, y, and extracted values.

## Details

**The dataset**  
Road density at 1 km grid resolution over the globe, derived from the
OpenStreetMap database (<https://www.openstreetmap.org/>) accessed
through the GeoFabrik (<https://www.geofabrik.de/>) functionalities, as
of 30 January 2026. Out of the original OpenStreetMap categories, roads
were classified into five classes:

- **class1** - highway (sum of the original categories "motorway" and
  "motorway_link")

- **class2** - primary (sum of "primary", "primary_link", "trunk" and
  "trunk_link")

- **class3** - secondary (sum of "secondary" and "secondary_link")

- **class4** - tertiary (sum of "tertiary" and "tertiary_link")

- **class5** - quaternary and other (sum of "residential",
  "living_street", "unknown" and "unclassified")

These five classes are further grouped into three aggregated layers:

- **primary** - sum of classes 4 and 5

- **other** - sum of classes 1, 2 and 3

- **all** - sum of all five classes

Note that the aggregated layer `"primary"` is a group of the minor-road
classes (4 and 5) and is *not* the same as the single class `"class2"`
(the OpenStreetMap "primary" category), while the aggregated layer
`"other"` groups the major-road classes (1, 2 and 3). In the three
aggregated layers, cells with no road cover are already stored as `NA`
(rather than 0) to streamline download and analyses; the five
single-class layers keep their original values.

All layers report the length in metres of roads within each ~1 km cell
(i.e. road density per grid cell).

**Available variables** (working synonyms in parentheses):

- "class1" ("class 1", "roads 1", "road class 1", "highways", "highway",
  "motorway")

- "class2" ("class 2", "roads 2", "road class 2", "primary class",
  "trunk")

- "class3" ("class 3", "roads 3", "road class 3", "secondary roads",
  "secondary")

- "class4" ("class 4", "roads 4", "road class 4", "tertiary roads",
  "tertiary")

- "class5" ("class 5", "roads 5", "road class 5", "quaternary", "local
  roads", "local", "residential")

- "primary" ("primary roads", "primary group", "classes 4 and 5")

- "other" ("other roads", "other group", "classes 1 2 and 3")

- "all" ("all roads", "total", "total roads", "all classes", "combined")

If \`vars\` is not specified, only "all" is downloaded.

**Data source:**  
Data are hosted in an embargoed Figshare repository and are retrieved
through private links. Access is provided for the use of this package
while the repository is under embargo; please check with the data
authors before redistributing the layers.

Note: Data extent is \[-180, 180, -60, 84\].

## Examples

``` r
# \donttest{
# Example 1: Download total road density for Italy
processed <- par_set(country = "Italy", crs = 3035) %>%
roads()

# Example 2: Download single road classes
processed <- par_set(country = "Italy", crs = 3035) %>%
roads(vars = c("highways", "class5"))

# Example 3: Download the two aggregated groups of classes
processed <- par_set(country = "Italy", crs = 3035) %>%
roads(vars = c("primary", "other"))
  # }
```
