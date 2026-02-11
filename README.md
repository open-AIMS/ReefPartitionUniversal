The ReefPartitionAllenAtlas R package provides functions for partitioning
coral reef spatial areas into smaller sites (such as for logistic and monitoring
purposes) based on raster data layers. The package provides functions for extracting
pixel points on a reef based on habitat types, extracting values for additional
raster layers such as bathymetry. Pixels can then be clustered into sites within
habitat types based on their geographic attributes as well as additional variable
values such as pixel depth.

The package uses a flexible framework allowing multiple additional raster layers
to be used, along with user defined pixel clustering algorithms. The package itself
defines two spatial clustering algorithm options, using adespatial::constr.hclust
[(Guénard and Legendre, 2022)](https://www.jstatsoft.org/article/view/v103i07) and spdep::skater [(AssunÇão et al. 2007)](https://www.tandfonline.com/doi/full/10.1080/13658810600665111#d1e2053).

# Installation
```R
# Install package using GitHub repo
remotes::install_github("open-AIMS/ReefPartitionUniversal", ref="package-template")
```

The package combines functionality from raster and vector data processing packages.
Dependencies include: sf, terra, sfnetworks, h3, igraph, spdep, adespatial, dplyr 
and magrittr

# Basic demonstration

```R
library(ReefPartitionAllenAtlas)

# Load input data (ensuring all are use the same CRS)
target_reef <- sf::st_read("target_reef.gpkg") # Defines the spatial extent of the reef
habitat <- terra::rast("habitat_raster.tif") # Defines the habitat pixels to extract from
bathymetry <- terra::rast("bathymetry_raster.tif") # Contains additional variable values for extraction and clustering

habitat_categories <- c(1, 10, 20) # Values for pixels to select from `habitat`

# Extract pixel values from raster layers
pixels <- extract_pixel_points(target_reef, habitat, bathymetry, habitat_categories)
pixels <- pixels[!is.na(pixels$depth), ]
pixels$UNIQUE_ID <- "ReefOne"

# Cluster pixels using adespatial::constr.hclust algorithm
# The
mst_hclust_pixels <- cluster_reef_pixels(pixels)

# Collate pixels from each site/cluster into polygons
mst_hclust_sites <- clustered_pixels_to_polygons(mst_hclust_pixels)
```

# License

This repository is licensed under MIT License.

# Development

Any problems and/or suggestions encountered with this package can be logged in
as GitHub issues.

This R package follows the [tidyverse styleguide](https://style.tidyverse.org/).

## Formatting

Code in this package can be auto-formatted to follow the tidyverse styleguide using
a formatter such as [Air](https://posit-dev.github.io/air/). Once installed for
the chosen IDE, using Air commands will reformat R files, modifying the whitespace,
linespace and punctuation to follow the tidyverse styleguide.

Additionally, Air has been set up to check file formatting when a new Pull Request
is made.

