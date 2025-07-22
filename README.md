# <div align="center">

# 

# <img src="envar/logo.svg" width="200" alt="envar logo">

# 

# \# envar

# 

# \### 🌍 Download and Process Environmental Variables for Species Distribution Modeling

# 

# \[!\[R-CMD-check](https://github.com/yourusername/envar/workflows/R-CMD-check/badge.svg)](https://github.com/yourusername/envar/actions)

# \[!\[CRAN status](https://www.r-pkg.org/badges/version/envar)](https://CRAN.R-project.org/package=envar)

# \[!\[License: GPL-3](https://img.shields.io/badge/License-GPL%203-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# \[!\[DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXXX)

# 

# \[Installation](#installation) • \[Quick Start](#quick-start) • \[Data Sources](#data-sources) • \[Examples](#examples) • \[Citation](#citation)

# 

# </div>

# 

# ---

# 

# \## 📋 Table of Contents

# 

# \- \[Overview](#overview)

# \- \[Installation](#installation)

# \- \[Quick Start](#quick-start)

# \- \[Data Sources](#data-sources)

# &nbsp; - \[Climate Data](#climate-data)

# &nbsp; - \[Land Cover](#land-cover)

# &nbsp; - \[Topography \& Terrain](#topography--terrain)

# &nbsp; - \[Soil Properties](#soil-properties)

# &nbsp; - \[Vegetation Indices](#vegetation-indices)

# &nbsp; - \[Hydrological Variables](#hydrological-variables)

# &nbsp; - \[Other Environmental Variables](#other-environmental-variables)

# \- \[Detailed Usage](#detailed-usage)

# \- \[Advanced Features](#advanced-features)

# \- \[Performance Tips](#performance-tips)

# \- \[Contributing](#contributing)

# \- \[Citation](#citation)

# \- \[License](#license)

# 

# \## 🌟 Overview

# 

# \*\*envar\*\* is a comprehensive R package designed to streamline the acquisition and processing of environmental variables for Species Distribution Modeling (SDM) and ecological research. It provides a unified interface to access multiple global environmental datasets, automatically handling downloading, cropping, resampling, and standardization.

# 

# \### Key Features

# 

# \- 🔄 \*\*Unified Interface\*\*: Single function to access 15+ environmental data sources

# \- 🌐 \*\*Global Coverage\*\*: Access worldwide environmental data at multiple resolutions

# \- ⚡ \*\*Automatic Processing\*\*: Intelligent cropping, resampling, and masking to your study area

# \- 🎯 \*\*SDM-Ready\*\*: Output format optimized for species distribution modeling workflows

# \- 📦 \*\*Smart Caching\*\*: Avoid redundant downloads with intelligent file management

# \- 🛠️ \*\*Flexible\*\*: Support for multiple spatial formats (sf, terra, coordinates)

# \- 📊 \*\*Multi-source Integration\*\*: Combine variables from different sources seamlessly

# 

# \## 📥 Installation

# 

# \### From CRAN (when available)

# ```r

# install.packages("envar")

# ```

# 

# \### Development version from GitHub

# ```r

# \# install.packages("remotes")

# remotes::install\_github("yourusername/envar")

# ```

# 

# \### Dependencies

# The package requires the following R packages:

# ```r

# \# Core spatial packages

# install.packages(c("terra", "sf", "exactextractr"))

# 

# \# Data access

# install.packages(c("httr", "rnaturalearth", "rnaturalearthdata"))

# 

# \# Utilities

# install.packages(c("cli", "fs"))

# ```

# 

# \## 🚀 Quick Start

# 

# ```r

# library(envar)

# 

# \# Download bioclimatic variables for Italy

# bio\_italy <- var\_get(

# &nbsp; extent = "Italy",

# &nbsp; source = "worldclim",

# &nbsp; variables = "bioclim"

# )

# 

# \# Plot the result

# plot(bio\_italy)

# 

# \# Download multiple environmental layers

# env\_data <- var\_get(

# &nbsp; extent = "Italy",

# &nbsp; source = c("worldclim", "cloud\_topo", "ndvi"),

# &nbsp; variables = list(

# &nbsp;   worldclim = "bioclim",

# &nbsp;   cloud\_topo = c("cloud", "elevation"),

# &nbsp;   ndvi = "ndvi\_mean"

# &nbsp; )

# )

# ```

# 

# \## 📊 Data Sources

# 

# \### Climate Data

# 

# \#### WorldClim 2.1

# Global climate and bioclimatic variables at 30s to 10m resolution.

# 

# ```r

# \# Available variables

# var\_explore(source = "worldclim", what = "variables")

# ```

# 

# | Variable | Description | Units |

# |----------|-------------|-------|

# | `bioclim` | 19 bioclimatic variables (bio1-bio19) | Various |

# | `tmean` | Monthly mean temperature | °C |

# | `tmin` | Monthly minimum temperature | °C |

# | `tmax` | Monthly maximum temperature | °C |

# | `prec` | Monthly precipitation | mm |

# | `srad` | Solar radiation | kJ m⁻² day⁻¹ |

# | `wind` | Wind speed | m s⁻¹ |

# | `vapr` | Water vapor pressure | kPa |

# 

# \#### CHELSA v2.1

# High-resolution climate data with better representation of orographic effects.

# 

# ```r

# \# Current climate

# current\_climate <- var\_get(

# &nbsp; extent = "Switzerland",

# &nbsp; source = "chelsa",

# &nbsp; variables = c("tas", "pr")

# )

# ```

# 

# \#### CHELSA CMIP5

# Future climate projections from CMIP5 models.

# 

# ```r

# \# Future projections

# future\_climate <- var\_get(

# &nbsp; extent = "Switzerland",

# &nbsp; source = "chelsa\_cmip5",

# &nbsp; variables = c("tas", "pr"),

# &nbsp; model = "MIROC5",

# &nbsp; scenario = "rcp45",

# &nbsp; period = "2041-2060"

# )

# ```

# 

# \#### CHELSA-Bioclim+

# Extended set of bioclimatic variables including growing degree days and snow cover.

# 

# ```r

# extended\_bioclim <- var\_get(

# &nbsp; extent = "Alps",

# &nbsp; source = "chelsa\_bioclimplus",

# &nbsp; variables = c("bioclimplus", "gdd5", "nfd")

# )

# ```

# 

# \### Land Cover

# 

# \#### ESA Land Cover CCI

# Annual land cover maps at 300m resolution from 1992-2020.

# 

# ```r

# landcover <- var\_get(

# &nbsp; extent = "Kenya",

# &nbsp; source = "esa\_landcover",

# &nbsp; variables = c("landcover", "forest", "cropland"),

# &nbsp; year = 2020

# )

# ```

# 

# \#### Consensus Land Cover

# Integrative land cover product combining multiple global datasets.

# 

# ```r

# consensus\_lc <- var\_get(

# &nbsp; extent = bbox\_object,

# &nbsp; source = "consensus\_landcover",

# &nbsp; variables = c("evergreen\_broadleaf", "cultivated", "urban")

# )

# ```

# 

# \### Topography \& Terrain

# 

# \#### Cloud Cover and Topography

# EarthEnv cloud cover frequency and terrain variables.

# 

# ```r

# terrain <- var\_get(

# &nbsp; extent = "Nepal",

# &nbsp; source = "cloud\_topo",

# &nbsp; variables = c("cloud", "elevation", "slope", "aspect", "roughness")

# )

# ```

# 

# \### Soil Properties

# 

# \#### Harmonized World Soil Database (HWSD)

# Global soil properties at 1km resolution.

# 

# ```r

# soil <- var\_get(

# &nbsp; extent = "Brazil",

# &nbsp; source = "hwsd",

# &nbsp; variables = c("sand", "clay", "organic\_carbon", "ph", "cec")

# )

# ```

# 

# | Variable | Description | Units |

# |----------|-------------|-------|

# | `sand`, `silt`, `clay` | Soil texture fractions | % |

# | `organic\_carbon` | Organic carbon content | % weight |

# | `ph` | Soil pH | - |

# | `cec` | Cation exchange capacity | cmol/kg |

# | `bulk\_density` | Bulk density | kg/m³ |

# 

# \### Vegetation Indices

# 

# \#### NDVI Time Series

# Normalized Difference Vegetation Index from MODIS or GIMMS.

# 

# ```r

# vegetation <- var\_get(

# &nbsp; extent = sf\_polygon,

# &nbsp; source = "ndvi",

# &nbsp; variables = c("ndvi\_mean", "ndvi\_amplitude", "greenup\_date"),

# &nbsp; ndvi\_source = "modis",

# &nbsp; year = 2022

# )

# ```

# 

# \#### SPECTRE Heterogeneity

# Spectral heterogeneity metrics for biodiversity modeling.

# 

# ```r

# heterogeneity <- var\_get(

# &nbsp; extent = "Madagascar",

# &nbsp; source = "spectre",

# &nbsp; variables = c("shannon", "cv", "contrast")

# )

# ```

# 

# \### Hydrological Variables

# 

# \#### Freshwater Variables

# HydroSHEDS-based hydrological variables.

# 

# ```r

# hydro <- var\_get(

# &nbsp; extent = "Amazon",

# &nbsp; source = "freshwater",

# &nbsp; variables = c("flow\_accumulation", "stream\_distance", "basin")

# )

# ```

# 

# \### Other Environmental Variables

# 

# \#### Global Aridity Index

# Aridity and potential evapotranspiration data.

# 

# ```r

# aridity <- var\_get(

# &nbsp; extent = coords\_df,

# &nbsp; source = "aridity",

# &nbsp; variables = c("aridity", "pet")

# )

# ```

# 

# \#### Global Wind Atlas

# Wind speed and power density at various heights.

# 

# ```r

# wind <- var\_get(

# &nbsp; extent = "Denmark",

# &nbsp; source = "wind",

# &nbsp; variables = c("wind\_speed", "power\_density"),

# &nbsp; height = "100"  # 100m height

# )

# ```

# 

# \#### Climate Stability

# Long-term climate stability indices.

# 

# ```r

# stability <- var\_get(

# &nbsp; extent = "Borneo",

# &nbsp; source = "climate\_stability",

# &nbsp; variables = c("temperature\_stability", "precipitation\_stability")

# )

# ```

# 

# \## 📖 Detailed Usage

# 

# \### Specifying Spatial Extent

# 

# envar accepts multiple formats for defining your area of interest:

# 

# ```r

# \# 1. Country or continent name

# data <- var\_get(extent = "France", ...)

# data <- var\_get(extent = "South America", ...)

# 

# \# 2. SF object (shapefile, etc.)

# shp <- sf::st\_read("study\_area.shp")

# data <- var\_get(extent = shp, ...)

# 

# \# 3. Coordinate matrix for point extraction

# coords <- matrix(c(10.5, 45.8, 11.2, 46.1), ncol = 2, byrow = TRUE)

# data <- var\_get(extent = coords, ...)

# 

# \# 4. Terra extent object

# ext <- terra::ext(c(xmin = 5, xmax = 15, ymin = 45, ymax = 50))

# data <- var\_get(extent = ext, ...)

# ```

# 

# \### Buffer Around Extent

# 

# Add a buffer around your study area:

# 

# ```r

# \# 50km buffer

# data <- var\_get(

# &nbsp; extent = "Switzerland",

# &nbsp; buffer\_km = 50,

# &nbsp; source = "worldclim",

# &nbsp; variables = "bioclim"

# )

# ```

# 

# \### Multiple Sources

# 

# Download from multiple sources simultaneously:

# 

# ```r

# multi\_env <- var\_get(

# &nbsp; extent = "Italy",

# &nbsp; source = c("worldclim", "cloud\_topo", "ndvi", "hwsd"),

# &nbsp; variables = list(

# &nbsp;   worldclim = c("bio1", "bio12"),

# &nbsp;   cloud\_topo = c("cloud", "elevation"),

# &nbsp;   ndvi = "ndvi\_mean",

# &nbsp;   hwsd = c("clay", "ph")

# &nbsp; ),

# &nbsp; output\_file = list(

# &nbsp;   worldclim = "italy\_climate.tif",

# &nbsp;   cloud\_topo = "italy\_terrain.tif",

# &nbsp;   ndvi = "italy\_vegetation.tif",

# &nbsp;   hwsd = "italy\_soil.tif"

# &nbsp; )

# )

# ```

# 

# \### Saving Output

# 

# ```r

# \# Single file

# data <- var\_get(

# &nbsp; extent = "Portugal",

# &nbsp; source = "worldclim",

# &nbsp; variables = "bioclim",

# &nbsp; output\_file = "portugal\_bioclim.tif"

# )

# 

# \# Multiple files for multiple sources

# data <- var\_get(

# &nbsp; extent = "Portugal",

# &nbsp; source = c("worldclim", "ndvi"),

# &nbsp; variables = list(

# &nbsp;   worldclim = "bioclim",

# &nbsp;   ndvi = "ndvi\_mean"

# &nbsp; ),

# &nbsp; output\_file = list(

# &nbsp;   worldclim = "portugal\_climate.tif",

# &nbsp;   ndvi = "portugal\_ndvi.tif"

# &nbsp; )

# )

# ```

# 

# \## 🔧 Advanced Features

# 

# \### Custom Resolution

# 

# All data is automatically resampled to 30 arc-seconds by default, but original resolution can be specified:

# 

# ```r

# \# Download at 5 arc-minutes resolution

# coarse\_data <- var\_get(

# &nbsp; extent = "Europe",

# &nbsp; source = "worldclim",

# &nbsp; resolution = "5m",

# &nbsp; variables = "bioclim"

# )

# ```

# 

# \### Exploring Available Options

# 

# ```r

# \# See all available sources

# var\_explore()

# 

# \# Check variables for a specific source

# var\_explore(source = "chelsa", what = "variables")

# 

# \# Check available resolutions

# var\_explore(what = "resolutions")

# ```

# 

# \### Error Handling

# 

# The package provides informative error messages with suggestions:

# 

# ```r

# \# This will show available variables for worldclim

# data <- var\_get(

# &nbsp; extent = "Italy",

# &nbsp; source = "worldclim",

# &nbsp; variables = "invalid\_var"  # Error with helpful suggestions

# )

# ```

# 

# \## ⚡ Performance Tips

# 

# 1\. \*\*Use specific variables\*\*: Instead of downloading all variables, specify only what you need

# &nbsp;  ```r

# &nbsp;  # Good

# &nbsp;  var\_get(extent, variables = c("bio1", "bio12"))

# &nbsp;  

# &nbsp;  # Less efficient

# &nbsp;  var\_get(extent, variables = "bioclim")  # Downloads all 19 variables

# &nbsp;  ```

# 

# 2\. \*\*Leverage caching\*\*: The package caches downloads in a temporary directory during the session

# 

# 3\. \*\*Batch processing\*\*: Download multiple sources in one call rather than separate calls

# 

# 4\. \*\*Appropriate resolution\*\*: Use coarser resolutions for large extents

# 

# \## 🤝 Contributing

# 

# We welcome contributions! Please see our \[Contributing Guidelines](CONTRIBUTING.md) for details.

# 

# \### Reporting Issues

# \- Use \[GitHub Issues](https://github.com/yourusername/envar/issues)

# \- Include a minimal reproducible example

# \- Specify your R version and package versions

# 

# \### Adding New Data Sources

# To add a new data source:

# 1\. Create a new `var\_get\_\[source].R` file

# 2\. Update `validate\_inputs.R`

# 3\. Add documentation and examples

# 4\. Submit a pull request

# 

# \## 📚 Citation

# 

# If you use envar in your research, please cite:

# 

# ```bibtex

# @software{envar,

# &nbsp; author = {Your Name},

# &nbsp; title = {envar: Download and Process Environmental Variables for SDM},

# &nbsp; year = {2024},

# &nbsp; url = {https://github.com/yourusername/envar},

# &nbsp; version = {0.1.0}

# }

# ```

# 

# \### Data Source Citations

# 

# When using specific data sources, please also cite:

# 

# \*\*WorldClim 2.1\*\*: Fick, S.E. and Hijmans, R.J. (2017). WorldClim 2: new 1‐km spatial resolution climate surfaces for global land areas. \*International Journal of Climatology\*, 37(12), 4302-4315.

# 

# \*\*CHELSA\*\*: Karger, D.N. et al. (2017). Climatologies at high resolution for the earth's land surface areas. \*Scientific Data\*, 4, 170122.

# 

# \*\*ESA Land Cover\*\*: ESA Climate Change Initiative - Land Cover project 2017.

# 

# \*\*Consensus Land Cover\*\*: Tuanmu, M.N. \& Jetz, W. (2014). A global 1‐km consensus land‐cover product for biodiversity and ecosystem modelling. \*Global Ecology and Biogeography\*, 23(9), 1031-1045.

# 

# \*\*HWSD\*\*: FAO/IIASA/ISRIC/ISSCAS/JRC (2012). Harmonized World Soil Database (version 1.2).

# 

# \[See full citation list](CITATIONS.md)

# 

# \## 📄 License

# 

# This package is licensed under GPL-3. See \[LICENSE](LICENSE) for details.

# 

# ---

# 

# <div align="center">

# 

# \*\*envar\*\* - Making environmental data accessible for ecological research

# 

# \[Report Bug](https://github.com/yourusername/envar/issues) • \[Request Feature](https://github.com/yourusername/envar/issues) • \[Documentation](https://yourusername.github.io/envar/)

# 

# </div>

